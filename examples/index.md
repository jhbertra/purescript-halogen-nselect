# NSelect

NSelect is a select library for PureScript Halogen. It manages some common logic to make building components like typeahead or dropdown easier. How your component looks is completely controlled by you.

This document assumes you are already familiar with PureScript and Halogen. The type signatures in this document are for demonstration purpose only, they are inaccurate because many type variables are ommited.

## Introduction

Every Halogen component has a type signature like this:

```hs
component :: H.Component HH.HTML Query Props Message m
```

- `Query` defines what actions can be sent to this component from the outside (parent component).
- `Props` defines what data this component is accepting, also called `input` in other documentations. It's similar to the props in React, but not exactly the same.
- `Message` defines what events this component emits to the outside (parent component), also called `output` in other documentations.

In the case of `NSelect.component`, `Props` is more interesting than the other two.

```hs
-- Excerpt from module NSelect

type State =
  { isOpen :: Boolean
  , highlightedIndex :: Int
  }

type Props =
  { render :: State -> HTML
  , itemCount :: Int
  }
```

`Props` is a record of two fields, let's ignore the `itemCount` field and focus on the `render` field. You can see from the type signature, `render` field is a function that takes `State` and returns `HTML`. This means when using `NSelect`, you need to pass a `render` function to `NSelect`.

And if you take a look at the definition of `NSelect.render`:

```hs
render :: InnerState -> HTML
render state =
  state.props.render $ innerStateToState state
```

`NSelect` doesn't render anything more than you passed in. That's why at the beginning of this document, we said

> How the component looks is completely controlled by you.

## A dropdown example

Let's use `NSelect` to build a simple dropdown. The `purescript-halogen-nselect` package has only one module called `NSelect`, first `import NSelect as NSelect`.

```hs
module Example.Dropdown where

import NSelect as Select
```

Then, let's write the `render` function and fill in the missing parts piece by piece.

```hs
render :: State -> HTML
render state =
  HH.slot _dropdown unit Select.component
    { render: renderSelect state
    , itemCount: 0
    } $ Just <<< HandleDropdown

```

The standard way in Halogen to render a child component is by using `HH.slot`. As said above, we need to pass a `render` function to `NSelect`. Here, we pass `renderSelect` to `Select.component`.

```hs
renderSelect :: State -> Select.State -> Select.HTML
renderSelect state st =
  HH.div (Select.setRootProps []) $ join
  [ -- A button to open/close dropdown
    pure $ HH.button
    ( Select.setToggleProps [])
    [ HH.text $ if st.isOpen then "Close" else "Open" ]
    -- The dropdown itself
  , guard st.isOpen $> HH.div_
    [ HH.input
      [ HP.value state.value
      , HE.onValueInput $ Just <<< Select.raise <<< OnInput
      ]
    , HH.div_
      [ HH.text $ "You typed: " <> state.value
      ]
    ]
  ]
```

Some observations:

- `Select.setRootProps` is used on the dropdown root element, so that click outside will close the dropdown.
- `Select.setToggleProps` is used on the toggle button, so that `NSelect` will update the `st.isOpen` value.
- Since `renderSelect` is passed to and rendered inside `NSelect`, it has different return type from `Example.Dropdown.render`. But you can still trigger `Example.Dropdown.Action` by using `Select.raise`.

Demo:

<div class="Demo">
  <example-dropdown></example-dropdown>
</div>

Click the button above should open a dropdown. So, with only a few lines of code, you get:

- A toggle button to open a dropdown, you don't need to manage the `open` state of dropdown
- Click outside to close the dropdown, you don't need to implement it repeatedly

## More examples

The source code can be found in the [examples](https://github.com/nonbili/purescript-halogen-nselect/tree/master/examples) folder.

### Render another component in dropdown

This example is similar to the above one, but this time, the input inside the dropdown is actually another component. This means you can embed any components furthur inside `NSelect`, as long as `Slot` types are correct.

<div class="Demo">
  <example-component-in-dropdown></example-component-in-dropdown>
</div>

### Autocomplete

Besides `Select.setRootProps`, this example uses three more helpers:

- `Select.setInputProps` allows `NSelect` to manage focus/blur event of `<input>`, to change `isOpen` state accordingly
- `Select.setMenuProps` allows `NSelect` to track the scroll position
- `Select.setItemProps` allows `NSelect` to emit `Selected` Message when user select an item

This example demonstrates:

- Press `ArrowUp`/`ArrowDown` to change selection, `NSelect` will make sure current highlighted item is visible
- Press `Enter` to select

<div class="Demo">
  <example-autocomplete></example-autocomplete>
</div>

### Two inputs

When using `Select.setInputProps`, `NSelect` manages `KeyDownEvent` of `<input>` for you, so that pressing `Enter` will select current highlighted item. Use `Select.setInputProps'` to bind more shortcuts to your `<input>`.

This example demonstrates using `Tab` key to select an item in the first input dropdown and focus the second input.

<div class="Demo">
  <example-two-inputs></example-two-inputs>
</div>

## Acknowledgement

This library was inspired by [downshift](https://github.com/downshift-js/downshift) and [purescript-halogen-select](https://github.com/citizennet/purescript-halogen-select).
