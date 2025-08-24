//go:build plan9
// +build plan9

// Copyright 2025 The TCell Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use file except in compliance with the License.
// You may obtain a copy of the license at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.


package tcell

import (
    draw "9fans.net/go/draw"
    "errors"
    "image"
    "sync"
    "time"
)

// drawScreen implements tcell.Screen using Plan 9's libdraw via 9fans.net/go/draw.
// It renders a cell grid into the window (Display.ScreenImage) and translates
// mouse/keyboard/resize events to tcell events.

type drawScreen struct {
    d      *draw.Display
    screen *draw.Image // same as d.ScreenImage
    font   *draw.Font

    cellW int // pixel width of one cell (roughly glyph width of "M")
    cellH int // pixel height (font height)

    width  int // cells in X
    height int // cells in Y

    colorCache map[uint32]*draw.Image // solid 1×1 replicated color images (by 0xRRGGBBAA)
    cells      []cell                 // back buffer of cells

    defStyle Style

    // input controllers
    kbd *draw.Keyboardctl
    mou *draw.Mousectl

    evq chan Event // tcell event queue

    // cursor state
    cx, cy        int
    cursorVisible bool

    // feature flags
    mouseEnabled bool
    pasteEnabled bool

    mu sync.Mutex
}

type cell struct {
    ch    rune
    comb  []rune
    style Style
}

// ---- Lifecycle -------------------------------------------------------------

// newDrawScreen returns a Screen backed by Plan 9 draw(3).
func newDrawScreen() (Screen, error) {
    s := &drawScreen{}
    if err := s.Init(); err != nil {
        return nil, err
    }
    return s, nil
}

func (s *drawScreen) Init() error {
    // Initialize draw(3) window. The display creates a single host window.
    d, err := draw.Init(nil, "", "tcell", "")
    if err != nil {
        return err
    }
    s.d = d
    s.screen = d.ScreenImage
    s.font = d.Font

    // Keyboard & mouse controllers.
    s.kbd = d.InitKeyboard()
    s.mou = d.InitMouse()

    s.colorCache = make(map[uint32]*draw.Image, 64)
    s.evq = make(chan Event, 64)

    // Measure cell metrics using the default font.
    s.cellH = s.font.Height
    // Use width of a wide glyph; StringSize returns logical size.
    if pt := s.font.StringSize("M"); pt.X > 0 {
        s.cellW = pt.X
    } else {
        s.cellW = s.cellH // fallback: square cells
    }

    s.recomputeGrid()

    // Default style: terminal default (let app override)
    s.defStyle = StyleDefault

    // Clear the screen image.
    s.Clear()
    s.d.Flush()
    return nil
}

func (s *drawScreen) Fini() {
    if s == nil || s.d == nil {
        return
    }
    // Try to free resources politely.
    for _, img := range s.colorCache {
        _ = img.Free()
    }
    // NOTE: ScreenImage is owned by the Display; Display.Close() will tear it down.
    // We intentionally rely on s.d.Close() instead of:
    // _ = s.screen.Free()
    s.d.Close()
}

// ---- Screen interface essentials ------------------------------------------

func (s *drawScreen) Size() (int, int) { return s.width, s.height }

func (s *drawScreen) Colors() int { return 1 << 24 }

func (s *drawScreen) Style() Style { return s.defStyle }

func (s *drawScreen) SetStyle(st Style) { s.defStyle = st }

func (s *drawScreen) Clear() {
    s.mu.Lock()
    defer s.mu.Unlock()

    // Reset backing cells and paint window with default background.
    s.cells = make([]cell, s.width*s.height)

    // Fill bg
    _, bg, _ := s.defStyle.Decompose()
    s.fillRect(s.winRect(), s.colorOf(bg))
}

func (s *drawScreen) Fill(r rune, comb []rune, style Style) {
    s.mu.Lock()
    defer s.mu.Unlock()

    for y := 0; y < s.height; y++ {
        for x := 0; x < s.width; x++ {
            s.setCellLocked(x, y, r, comb, style, true)
        }
    }
}

func (s *drawScreen) SetContent(x, y int, mainc rune, comb []rune, style Style) {
    s.mu.Lock()
    defer s.mu.Unlock()
    s.setCellLocked(x, y, mainc, comb, style, true) // defer painting; Show() draws a coherent frame
}

func (s *drawScreen) GetContent(x, y int) (rune, []rune, Style, int) {
    s.mu.Lock()
    defer s.mu.Unlock()
    if x < 0 || y < 0 || x >= s.width || y >= s.height {
        return ' ', nil, s.defStyle, 1
    }
    c := s.cells[y*s.width+x]

    // Compute display width just for the primary rune.
    // (Combining marks in c.comb are width 0 by definition.)
    w := wcwidth(c.ch)
    if w < 1 || w > 2 {
        w = 1
    }
    return c.ch, cloneRunes(c.comb), c.style, w
}

// Full repaint Show: paint background, then all cells every frame.
func (s *drawScreen) Show() {
    s.mu.Lock()
    // Paint background first.
    _, bg, _ := s.defStyle.Decompose()
    s.fillRect(s.winRect(), s.colorOf(bg))

    // Draw all cells from backing store.
    for y := 0; y < s.height; y++ {
        for x := 0; x < s.width; x++ {
            s.drawCell(x, y, s.cells[y*s.width+x])
        }
    }
    s.mu.Unlock()
    s.d.Flush()
}

// Sync forces a repaint now.
func (s *drawScreen) Sync() { s.Show() }

func (s *drawScreen) ShowCursor(x, y int) {
    s.mu.Lock()
    s.cx, s.cy = x, y
    s.cursorVisible = true
    s.mu.Unlock()
    // Move the OS cursor to the cell's baseline (left).
    pt := s.cellToPx(x, y)
    s.d.MoveCursor(draw.Pt(pt.X, pt.Y+s.font.Ascent))
}

func (s *drawScreen) HideCursor() {
    s.mu.Lock()
    s.cursorVisible = false
    s.mu.Unlock()
}

func (s *drawScreen) EnableMouse()  { s.mouseEnabled = true }
func (s *drawScreen) DisableMouse() { s.mouseEnabled = false }
func (s *drawScreen) HasMouse() bool { return true }

func (s *drawScreen) EnablePaste()  { s.pasteEnabled = true }
func (s *drawScreen) DisablePaste() { s.pasteEnabled = false }

func (s *drawScreen) SetClipboard(text string) error {
    if !s.pasteEnabled {
        return errors.New("paste/clipboard disabled")
    }
    return draw.WriteSnarf(text)
}

func (s *drawScreen) GetClipboard() (string, error) {
    if !s.pasteEnabled {
        return "", errors.New("paste/clipboard disabled")
    }
    return draw.ReadSnarf()
}

// ---- Event loop ------------------------------------------------------------

func (s *drawScreen) PollEvent() Event {
    for {
        // Prefer any posted events first.
        select {
        case ev := <-s.evq:
            return ev
        default:
        }

        // Then block on input or resize.
        select {
        case r := <-s.kbd.C:
            return s.translateKey(r)

        case m := <-s.mou.C:
            // Update mouse snapshot as per draw docs.
            s.mou.Mouse = m
            if !s.mouseEnabled {
                continue
            }
            x := m.Point.X / s.cellW
            y := m.Point.Y / s.cellH

            // Mouse coordinates: clamp to grid (prevents odd callers from indexing out of bounds)
            if x < 0 { x = 0 } else if x >= s.width { x = s.width-1 }
            if y < 0 { y = 0 } else if y >= s.height { y = s.height-1 }

            btn := ButtonMask(0)
            // Buttons: bit 0 => Button1, bit 1 => Button2, bit 2 => Button3
            if m.Buttons&1 != 0 { btn |= Button1 }
            if m.Buttons&2 != 0 { btn |= Button2 }
            if m.Buttons&4 != 0 { btn |= Button3 }
            // Wheel (Plan 9 devdraw typically maps wheel to buttons 8/16)
            if m.Buttons&8 != 0  { btn |= WheelUp }
            if m.Buttons&16 != 0 { btn |= WheelDown }
            return NewEventMouse(x, y, btn, ModNone)

        case <-s.mou.Resize:
            // On resize, refresh screen image (some servers reattach) and rebuild color cache.
            // Hold the lock while mutating shared state to avoid races with SetContent/Show.
            s.mu.Lock()
            s.screen = s.d.ScreenImage
            for _, img := range s.colorCache { _ = img.Free() }
            s.colorCache = make(map[uint32]*draw.Image, 64)
            s.recomputeGrid()
            s.mu.Unlock()
            s.evq <- NewEventResize(s.width, s.height)
            s.Show() // repaint now
        }
    }
}

func (s *drawScreen) PostEvent(ev Event)          { s.evq <- ev }
func (s *drawScreen) PostEventWait(ev Event) bool { s.evq <- ev; return true }

// ---- Rendering -------------------------------------------------------------

func (s *drawScreen) drawCell(x, y int, c cell) {
    // Compute target rect; expand to two cells if the rune is wide and we have space.
    r := s.cellRect(x, y)
    w := wcwidth(c.ch)
    if w == 2 {
        if x+1 < s.width {
            r.Max.X += s.cellW
        } else {
            // If wide at last column, render as narrow (truncate visually) to avoid painting out of bounds.
            w = 1
        }
    }

    // Style & attributes (handle reverse here).
    fg, bg, attr := c.style.Decompose()
    if attr&AttrReverse != 0 {
        fg, bg = bg, fg
        attr &^= AttrReverse
    }

    // Paint background across the (possibly widened) rect.
    s.fillRect(r, s.colorOf(bg))

    // If there's nothing to draw on top, we're done.
    if c.ch == 0 && len(c.comb) == 0 {
        return
    }

    // Choose foreground paint.
    src := s.colorOf(fg)

    // Build rune slice: base + combining marks. If base is empty, use space to carry marks.
    rs := make([]rune, 0, 1+len(c.comb))
    base := c.ch
    if base == 0 && len(c.comb) > 0 {
        // No base; draw combining marks over a space so they have a baseline.
        base = ' '
    }
    // If base is a space and there are no combining marks, we would have returned above.
    rs = append(rs, base)
    if len(c.comb) > 0 {
        rs = append(rs, c.comb...)
    }

    // Draw text baseline-aligned at the left edge of the (possibly widened) cell.
    pt := draw.Pt(r.Min.X, r.Min.Y+s.font.Ascent)
    s.screen.RunesBg(pt, src, image.Point{}, s.font, rs, s.colorOf(bg), image.Point{})

    // Optional polish: underline (1px) when requested.
    if attr&AttrUnderline != 0 {
        yline := r.Max.Y - 1
        s.screen.Line(draw.Pt(r.Min.X, yline), draw.Pt(r.Max.X, yline), 0, 0, 0, src, image.Point{})
    }
}

// ---- Key translation -------------------------------------------------------

func (s *drawScreen) translateKey(r rune) Event {
    const keyFn = 0xF000

    // Quick path: ASCII ^V (0x16) -> paste when enabled.
    if s.pasteEnabled && r == 0x16 { // Ctrl+V
        if txt, err := draw.ReadSnarf(); err == nil && len(txt) > 0 {
            return NewEventPaste([]byte(txt)) // if your tcell needs string: NewEventPaste(txt)
        }
        // fall through to regular ^V if snarf empty or error
    }

    // Special keys (and their modifiers) live in the 0xF000 range.
    if r >= keyFn && r <= keyFn+0x1FF {
        code := int(r - keyFn)
        mods := ModNone

        // NOTE: devdraw encodes modifiers in the low bits of the private-use key code.
        // The 0x20/0x40/0x80 mapping (Shift/Ctrl/Alt) matches common builds;
        // if your devdraw differs, adjust these masks here.
        if code&0x20 != 0 { mods |= ModShift; code &^= 0x20 } // Shift
        if code&0x40 != 0 { mods |= ModCtrl;  code &^= 0x40 } // Ctrl
        if code&0x80 != 0 { mods |= ModAlt;   code &^= 0x80 } // Alt

        // Paste via Shift+Insert (rio/devdraw convention)
        if s.pasteEnabled && code == 0x0D && (mods&ModShift) != 0 { // Insert + Shift
            if txt, err := draw.ReadSnarf(); err == nil && len(txt) > 0 {
                return NewEventPaste([]byte(txt)) // if needed: NewEventPaste(txt)
            }
            // if empty, continue and return a normal Insert+Shift event
        }

        // Function keys: 0x01..0x0C => F1..F12
        if code >= 0x01 && code <= 0x0C {
            key := Key(int(KeyF1) + (code - 1))
            return NewEventKey(key, 0, mods)
        }

        switch code {
        case 0x0D: // Insert
            return NewEventKey(KeyInsert, 0, mods)
        case 0x08: // Home
            return NewEventKey(KeyHome, 0, mods)
        case 0x09: // End
            return NewEventKey(KeyEnd, 0, mods)
        case 0x0B: // PgUp
            return NewEventKey(KeyPgUp, 0, mods)
        case 0x0C: // PgDn
            return NewEventKey(KeyPgDn, 0, mods)
        case 0x10: // Up
            return NewEventKey(KeyUp, 0, mods)
        case 0x11: // Left
            return NewEventKey(KeyLeft, 0, mods)
        case 0x12: // Right
            return NewEventKey(KeyRight, 0, mods)
        case 0x13: // Down
            return NewEventKey(KeyDown, 0, mods)
        case 0x7F: // Delete (forward)
            return NewEventKey(KeyDelete, 0, mods)
        }
        // Unknown special: pass through as a Rune with whatever modifiers we saw.
        return NewEventKey(KeyRune, r, mods)
    }

    // Plain runes and common ASCII controls.
    switch r {
    case '\n':
        return NewEventKey(KeyEnter, '\n', ModNone)
    case '\t':
        return NewEventKey(KeyTab, '\t', ModNone)
    case 0x1B: // ESC
        return NewEventKey(KeyEscape, 0, ModNone)
    case 0x08: // ^H backspace (common on Plan 9)
        return NewEventKey(KeyBackspace2, 0, ModNone)
    case 0x7F: // DEL used as backspace sometimes
        return NewEventKey(KeyBackspace, 0, ModNone)
    }

    // Map ^A..^Z to Ctrl-modified runes (useful for tty-like shortcuts).
    if r >= 0x01 && r <= 0x1A {
        ch := rune('a' + (r - 1))
        return NewEventKey(KeyRune, ch, ModCtrl)
    }

    // Default printable rune.
    return NewEventKey(KeyRune, r, ModNone)
}

// ---- Geometry helpers --------------------------------------------------------------

func (s *drawScreen) cellToPx(x, y int) image.Point {
    r := s.winRect()
    return image.Pt(r.Min.X+x*s.cellW, r.Min.Y+y*s.cellH)
}

func (s *drawScreen) cellRect(x, y int) draw.Rectangle {
    // Draw rectangles use their own Point/Rectangle but are compatible
    // with image.Point/image.Rectangle fields.
    p := s.cellToPx(x, y)
    return draw.Rpt(
        draw.Pt(p.X, p.Y),
        draw.Pt(p.X+s.cellW, p.Y+s.cellH),
    )
}

func (s *drawScreen) winRect() draw.Rectangle { return s.screen.R }

func (s *drawScreen) recomputeGrid() {
    r := s.screen.R
    oldW, oldH := s.width, s.height
    oldCells := s.cells

    s.width = max(1, r.Dx()/s.cellW)
    s.height = max(1, r.Dy()/s.cellH)

    s.cells = make([]cell, s.width*s.height)

    copyH := min(oldH, s.height)
    copyW := min(oldW, s.width)
    if oldW > 0 && copyH > 0 && copyW > 0 {
        for y := 0; y < copyH; y++ {
            copy(s.cells[y*s.width:y*s.width+copyW], oldCells[y*oldW:y*oldW+copyW])
        }
    }
    // Newly exposed edges will be painted by the next Show().
}

// ---- Rendering helpers -----------------------------------------------------

func (s *drawScreen) fillRect(r draw.Rectangle, color *draw.Image) {
    s.screen.Draw(r, color, nil, image.Point{})
}

func (s *drawScreen) colorOf(c Color) *draw.Image {
    // Resolve tcell Color to Plan 9 draw color image (solid, replicated).
    // If c is ColorDefault or invalid
    if !c.Valid() || c == ColorDefault {
            // Prefer the default background from defStyle to keep “default” consistent.
            _, bg, _ := s.defStyle.Decompose()
            if bg.Valid() && bg != ColorDefault {
                    return s.colorOf(bg) // recurse into cache path below
            }
            // Last resort if defStyle has no concrete bg.
            return s.d.Black
    }

    r8, g8, b8 := c.TrueColor().RGB()
    key := (uint32(r8)&0xFF)<<24 | (uint32(g8)&0xFF)<<16 | (uint32(b8)&0xFF)<<8 | 0xFF
    if img, ok := s.colorCache[key]; ok {
        return img
    }
    // Allocate a 1x1 solid image with replication, using the SCREEN'S PIX FORMAT.
    col := draw.Color(key)
    img, err := s.d.AllocImage(image.Rect(0, 0, 1, 1), s.screen.Pix, true, col)
    if err != nil {
        if !c.Valid() || c == ColorDefault {
            _, bg, _ := s.defStyle.Decompose()
            return s.colorOf(bg) // resolves to cached solid matching default bg
        }
        // Last resort if defStyle has no concrete bg.
        return s.d.White
    }
    s.colorCache[key] = img
    return img
}

// ---- Content set helper ----------------------------------------------------

func (s *drawScreen) setCellLocked(x, y int, mainc rune, comb []rune, style Style, deferDraw bool) {
    if x < 0 || y < 0 || x >= s.width || y >= s.height {
        return
    }
    i := y*s.width + x
    old := s.cells[i]
    newc := cell{ch: mainc, comb: cloneRunes(comb), style: style}

    if old.ch == newc.ch && old.style == newc.style && runesEqual(old.comb, newc.comb) {
        return
    }

    s.cells[i] = newc
    if !deferDraw {
        s.drawCell(x, y, newc)
    }
}

// runesEqual reports whether two rune slices are identical.
// Used to skip unnecessary writes to the backing store.
func runesEqual(a, b []rune) bool {
       if len(a) != len(b) {
               return false
       }
       for i := range a {
               if a[i] != b[i] {
                       return false
               }
       }
       return true
}

// ---- wcwidth ---------------------------------------------------------------

type rr struct{ lo, hi rune }

func inRange(r rune, tbl []rr) bool {
    lo, hi := 0, len(tbl)
    for lo < hi {
        m := (lo + hi) >> 1
        if r < tbl[m].lo {
            hi = m
        } else if r > tbl[m].hi {
            lo = m + 1
        } else {
            return true
        }
    }
    return false
}

// Common combining ranges (marks and zero-width format/variation selectors)
var combiningTbl = []rr{
    {0x0300, 0x036F}, // Combining Diacritical Marks
    {0x1AB0, 0x1AFF}, // Combining Diacritical Marks Extended
    {0x1DC0, 0x1DFF}, // Combining Diacritical Marks Supplement
    {0x20D0, 0x20FF}, // Combining Diacritical Marks for Symbols
    {0xFE20, 0xFE2F}, // Combining Half Marks
    {0x3099, 0x309A}, // Japanese combining marks
    {0x180B, 0x180D}, // Mongolian free variation selectors
    {0xFE00, 0xFE0F}, // Variation Selectors
    {0xE0100, 0xE01EF}, // Variation Selectors Supplement
    // Zero-width characters commonly encountered
    {0x200B, 0x200F}, // ZWSP/ZWNJ/ZWJ + dir marks
    {0x2060, 0x206F}, // word joiner & invisibles
    {0xFEFF, 0xFEFF}, // BOM / ZWNBSP
}

func isCombining(r rune) bool { return inRange(r, combiningTbl) }

// East Asian wide/fullwidth + emoji. This mirrors common terminal behavior.
var wideTbl = []rr{
    // CJK & Hangul blocks (wide)
    {0x1100, 0x115F}, // Hangul Jamo init
    {0x2329, 0x232A}, // 〈〉
    {0x2E80, 0x2FFB}, // CJK Radicals Supplement .. Ideographic Description
    {0x3000, 0x303E}, // CJK punctuation, space (U+3000)
    {0x3040, 0x30FF}, // Hiragana & Katakana
    {0x3100, 0x312F}, // Bopomofo
    {0x3130, 0x318F}, // Hangul Compatibility Jamo
    {0x3190, 0x33FF}, // Enclosed CJK, etc.
    {0x3400, 0x4DBF}, // CJK Ext A
    {0x4E00, 0x9FFF}, // CJK Unified
    {0xAC00, 0xD7A3}, // Hangul Syllables
    {0xF900, 0xFAFF}, // CJK Compatibility Ideographs
    {0xFE10, 0xFE19}, // Small form variants (vertical punctuation)
    {0xFE30, 0xFE6F}, // CJK Compatibility Forms
    {0xFF01, 0xFF60}, // Fullwidth ASCII variants
    {0xFFE0, 0xFFE6}, // Fullwidth symbols
    // Emoji & symbols that terminals commonly treat as width 2
    {0x1F300, 0x1F64F}, // Misc Symbols & Emoticons
    {0x1F680, 0x1F6FF}, // Transport & Map
    {0x1F900, 0x1F9FF}, // Supplemental Symbols & Pictographs
    {0x1FA70, 0x1FAFF}, // Symbols & Pictographs Extended-A
}

func isWide(r rune) bool { return inRange(r, wideTbl) }

// wcwidth returns the number of terminal cells a rune should occupy.
// 0 for combining/format controls, 1 for most runes, 2 for East Asian wide/fullwidth & emoji.
func wcwidth(r rune) int {
    // C0/C1 control characters
    if r == 0 || (r < 0x20) || (r >= 0x7F && r < 0xA0) {
        return 0
    }
    if isCombining(r) {
        return 0
    }
    if isWide(r) {
        return 2
    }
    return 1
}

// ---- Misc utilities --------------------------------------------------------

func cloneRunes(rs []rune) []rune {
    if len(rs) == 0 { return nil }
    out := make([]rune, len(rs))
    copy(out, rs)
    return out
}

func min(a, b int) int { if a < b { return a }; return b }
func max(a, b int) int { if a > b { return a }; return b }


// Optional capability: return the time when the screen backend was ready.
func (s *drawScreen) Context() interface{} { return struct{ When time.Time }{When: time.Now()} }

// --- tcell interfaces satisfied ---
var _ Screen = (*drawScreen)(nil)
