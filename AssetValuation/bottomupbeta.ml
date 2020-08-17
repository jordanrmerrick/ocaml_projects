open Core

let bottomupbeta unlevered taxrate debt equity =
  unlevered /. (1. +. (1. -. taxrate)) *. (debt /. equity)
