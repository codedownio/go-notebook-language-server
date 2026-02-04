package main

import (
	"encoding/json"
	"go/scanner"
	"go/token"
	"io"
	"os"
)

type Declaration struct {
	Type      string `json:"type"`
	StartLine int    `json:"start_line"`
	StartCh   int    `json:"start_ch"`
	EndLine   int    `json:"end_line"`
	EndCh     int    `json:"end_ch"`
}

type tokenInfo struct {
	pos token.Pos
	tok token.Token
	lit string
}

func main() {
	src, err := io.ReadAll(os.Stdin)
	if err != nil || len(src) == 0 {
		json.NewEncoder(os.Stdout).Encode([]Declaration{})
		return
	}

	fset := token.NewFileSet()
	file := fset.AddFile("stdin", fset.Base(), len(src))

	var s scanner.Scanner
	s.Init(file, src, nil, scanner.ScanComments)

	// Collect all tokens
	var tokens []tokenInfo
	for {
		pos, tok, lit := s.Scan()
		if tok == token.EOF {
			break
		}
		tokens = append(tokens, tokenInfo{pos, tok, lit})
	}

	decls := []Declaration{}
	braceDepth := 0
	parenDepth := 0
	atStmtStart := true

	i := 0
	for i < len(tokens) {
		t := tokens[i]

		// Skip comments — they don't affect statement boundaries or depth
		if t.tok == token.COMMENT {
			i++
			continue
		}

		// At the start of a new statement at depth 0, check for declaration keywords
		if atStmtStart && braceDepth == 0 && parenDepth == 0 {
			var decl *Declaration
			switch t.tok {
			case token.IMPORT:
				decl = scanDecl(fset, tokens, &i, &braceDepth, &parenDepth, "Import", false)
			case token.FUNC:
				decl = scanDecl(fset, tokens, &i, &braceDepth, &parenDepth, "Func", true)
			case token.TYPE:
				decl = scanDecl(fset, tokens, &i, &braceDepth, &parenDepth, "Type", true)
			case token.VAR:
				decl = scanDecl(fset, tokens, &i, &braceDepth, &parenDepth, "Var", false)
			case token.CONST:
				decl = scanDecl(fset, tokens, &i, &braceDepth, &parenDepth, "Const", false)
			}
			if decl != nil {
				decls = append(decls, *decl)
				atStmtStart = true
				continue
			}
		}

		// Not a declaration keyword (or not at statement start): skip through statement
		updateDepths(t.tok, &braceDepth, &parenDepth)

		if t.tok == token.SEMICOLON && braceDepth == 0 && parenDepth == 0 {
			atStmtStart = true
		} else {
			atStmtStart = false
		}

		i++
	}

	json.NewEncoder(os.Stdout).Encode(decls)
}

func updateDepths(tok token.Token, braceDepth, parenDepth *int) {
	switch tok {
	case token.LBRACE:
		*braceDepth++
	case token.RBRACE:
		*braceDepth--
	case token.LPAREN:
		*parenDepth++
	case token.RPAREN:
		*parenDepth--
	}
}

// scanDecl scans a declaration starting at tokens[*idx] (the keyword token).
// endAtBrace: if true, RBRACE at braceDepth==0 is an end condition (for func/type).
// If false, only SEMICOLON at depth 0 ends the declaration (for var/const/import).
// For grouped declarations (keyword followed by "("), RPAREN at parenDepth==0 ends it.
func scanDecl(fset *token.FileSet, tokens []tokenInfo, idx *int, braceDepth, parenDepth *int, declType string, endAtBrace bool) *Declaration {
	startPos := fset.Position(tokens[*idx].pos)
	decl := &Declaration{
		Type:      declType,
		StartLine: startPos.Line,
		StartCh:   startPos.Column,
		EndLine:   startPos.Line,
		EndCh:     startPos.Column,
	}

	*idx++ // consume keyword token

	// Check if the next non-comment token is ( → grouped declaration
	// (Only for non-func keywords; func never uses grouped syntax)
	grouped := false
	if !endAtBrace || declType == "Type" {
		for j := *idx; j < len(tokens); j++ {
			if tokens[j].tok == token.COMMENT {
				continue
			}
			if tokens[j].tok == token.LPAREN {
				grouped = true
			}
			break
		}
	}

	// For func, we also need to check for grouped, but func doesn't use (...)
	// grouping. However, we must NOT set grouped for func even though the
	// receiver "(r *Foo)" follows func.
	// The endAtBrace flag on func ensures we look for } not ).

	for *idx < len(tokens) {
		t := tokens[*idx]

		// Update depths before checking end conditions
		updateDepths(t.tok, braceDepth, parenDepth)

		endPos := fset.Position(t.pos)

		if grouped {
			// Grouped declaration: end at ) that brings parenDepth back to 0
			if t.tok == token.RPAREN && *parenDepth == 0 {
				decl.EndLine = endPos.Line
				decl.EndCh = endPos.Column
				*idx++
				// Skip trailing auto-inserted semicolon
				if *idx < len(tokens) && tokens[*idx].tok == token.SEMICOLON {
					*idx++
				}
				return decl
			}
		} else {
			// Non-grouped declaration
			if endAtBrace && t.tok == token.RBRACE && *braceDepth == 0 {
				// End at } for func body or type struct/interface body
				decl.EndLine = endPos.Line
				decl.EndCh = endPos.Column
				*idx++
				// Skip trailing auto-inserted semicolon
				if *idx < len(tokens) && tokens[*idx].tok == token.SEMICOLON {
					*idx++
				}
				return decl
			}
			if t.tok == token.SEMICOLON && *braceDepth == 0 && *parenDepth == 0 {
				// End at semicolon at depth 0 (all non-grouped declarations,
				// and bodyless func declarations)
				decl.EndLine = endPos.Line
				decl.EndCh = endPos.Column
				*idx++
				return decl
			}
		}

		*idx++
	}

	// Reached end of tokens without finding end condition
	if len(tokens) > 0 {
		endPos := fset.Position(tokens[len(tokens)-1].pos)
		decl.EndLine = endPos.Line
		decl.EndCh = endPos.Column
	}
	return decl
}
