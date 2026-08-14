test_that("any two omics layers satisfy the multi-omics input requirement", {
  layer <- data.frame(id = "x")

  expect_true(.mo_has_minimum_layers(list(layer, layer, NULL)))
  expect_true(.mo_has_minimum_layers(list(layer, NULL, layer)))
  expect_true(.mo_has_minimum_layers(list(NULL, layer, layer)))
  expect_true(.mo_has_minimum_layers(list(layer, layer, layer)))
})

test_that("zero or one omics layer does not satisfy the requirement", {
  layer <- data.frame(id = "x")

  expect_false(.mo_has_minimum_layers(list(NULL, NULL, NULL)))
  expect_false(.mo_has_minimum_layers(list(layer, NULL, NULL)))
  expect_equal(.mo_available_layer_count(list(layer, NULL, NULL)), 1L)
})
