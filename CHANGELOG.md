# CHANGELOG

# 0.1.3.0
* Transform workspace/symbol answers, which named the shadow document instead of the notebook

# 0.1.2.0
* Transform textDocument/selectionRange, textDocument/semanticTokens and the goto family, which reached the client in shadow coordinates
* Transform textDocument/rename, prepareRename, references, signatureHelp and declaration, which reached the server naming the notebook

# 0.1.1.2
* Untransform workspace/applyEdit, which named the shadow document and so was refused by the client

# 0.1.1.1
* Fix textDocument/formatting, which silently did nothing in 0.1.1.0. Formatting edits are now
  passed through untouched, so plain files format and notebooks are left alone.

# 0.1.1.0
* Support textDocument/inlayHint, textDocument/formatting and textDocument/onTypeFormatting
* Map code action edits back to the notebook instead of the shadow file
* Transform codeLens/resolve, so lens titles that the server defers resolve correctly

# 0.1.0.4
* Fix accidental dynamically linked Nix store paths.
