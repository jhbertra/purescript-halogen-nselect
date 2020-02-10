# NSelect

NSelect is a select library for PureScript Halogen. It manages the state of &lt;input> and helps you build components such as typeahead or dropdown. But how your component actually looks is completely controlled by you.

This document assumes you are already familiar with PureScript and Halogen. The type signatures in this document are for demonstration purpose only, they are inaccurate because type variables are ommited.

## Introduction

The `purescript-halogen-nselect` package has only one module called `NSelect`, first `import NSelect as NSelect`.

Every Halogen component has a type signature like this,

```haskell
component :: H.Component HH.HTML Query Props Message m
```

- `Query` defines what actions can be sent to this component from the outside (parent component).
- `Props` defines what data this component is accepting, also called `input` in other documentations. It's similar to the props in React, but not exactly the same.
- `Message` defines what events this component emit to the outside (parent component), also called `output` in other documentations.

In the case of `NSelect.component`,

```haskell
type Props =
  { render :: State -> HTML
  , itemCount :: Int
  }
```

`NSelect`

## A dropdown example

<example-dropdown></example-dropdown>

<example-component-in-dropdown></example-component-in-dropdown>

<example-autocomplete></example-autocomplete>

<example-two-inputs></example-two-inputs>
