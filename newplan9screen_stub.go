//go:build !plan9
// +build !plan9

package tcell

import "errors"

func NewPlan9Screen() (Screen, error) {
	return nil, errors.New("plan9 backend not available on this OS")
}
