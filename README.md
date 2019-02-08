# purescript-halogen-nselect

[![CircleCI](https://circleci.com/gh/nonbili/purescript-halogen-nselect.svg?style=svg)](https://circleci.com/gh/nonbili/purescript-halogen-nselect)

aNother Select library in purescript halogen, inspired by [purescript-halogen-select](https://github.com/citizennet/purescript-halogen-select) and [downshift](https://github.com/downshift-js/downshift).

Key differences from `purescript-halogen-select`:

1. `NSelect` doesn't use focus/blur of input to control visibility of dropdown, so that user can render anying inside dropdown. Related to [purescript-halogen-select#36](https://github.com/citizennet/purescript-halogen-select/issues/36)
2. `NSelect` doesn't care item's type, it only uses `itemCount`, so that ArrowUp/ArrowDown/Enter works
3. `NSelect` provides `setInputProps'`, so that user can easily handle more key bindings, like `Tab`, [an example](https://nonbili.github.io/purescript-halogen-nselect/#Two%20inputs)
