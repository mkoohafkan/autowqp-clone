#' Translate Form Data
#'
#' Replacement of Rank data
#'
#' @keywords internal
translate_metadata = function(form.data) {
#  "Standard: Dissolved Oxygen - fully saturated", "Standard: first point"
#  "Standard: DIW", "Zero-point"
#  "Standard: Specific Conductance - Dry", "Zero-point"
#  "Standard: Specific Conductance - Standard Solution", "Standard: first point"
#  "Standard: Specific Conductance - Standard Check", "Check: first point"
#  "Standard: Temperature - Water Bath", "Standard: first point"
#  "Standard: Turbidity - High", "Standard: first point"
#  "Standard: pH - 7", "Standard: first point"
#  "Standard: pH - 10", "Standard: second point"
#  "Standard: pH - 4", "Standard: third point"


  old.rank = form.data$result[[1]]$rank_name
  # zero-point
  zero.point = grepl("DIW", old.rank) |
    grepl("Dry", old.rank)
  # standard first point
  first.point = grepl("Standard Solution", old.rank) |
    grepl("fully saturated", old.rank) |
    grepl("Water Bath", old.rank) |
    grepl("Turbidity - High", old.rank) |
    grepl("pH - 7", old.rank)
  # standard second point
  second.point = grepl("pH - 10", old.rank)
  # standard third point
  third.point = grepl("pH - 4", old.rank)
  # check
  check.point = grepl("Standard Check", old.rank)

  new.rank = old.rank
  new.rank[zero.point] = "Zero-Point"
  new.rank[first.point] = "Standard: First Point"
  new.rank[second.point] = "Standard: Second Point"
  new.rank[third.point] = "Standard: Third Point"
  new.rank[check.point] = "Check: First Point"

  form.data$result[[1]]$rank_name = new.rank

  form.data
}
