app <- ShinyDriver$new("../../")
app$snapshotInit("overview_projection_2D_categorical")

app$setInputs(sidebar = "overview")
app$snapshot()
