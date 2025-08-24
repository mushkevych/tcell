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

func NewConsoleScreen() (Screen, error) {
    if s, err := newDrawScreen(); err == nil {
        return s, nil                   // prefer native draw(3)
    }
    return NewTerminfoScreen()          // fallback to vt(1)/terminfo path
}
