#' Mock MCDM Decision Matrix Data
#'
#' A dataset containing simulated responses from 20 experts/alternatives across
#' 13 items representing 4 criteria (Cost, Quality, Delivery, Sustainability).
#' Intended for use with `prepare_mcdm_data()`.
#'
#' @format A data frame with 20 rows and 13 variables:
#' \describe{
#'   \item{ExpertID}{Identifier for the respondent}
#'   \item{Alternative}{The object being evaluated}
#'   \item{cost_raw_mat}{Continuous cost of raw materials}
#'   \item{cost_labor}{Continuous cost of labor}
#'   \item{qual_durability}{Likert 1-5 quality score}
#'   \item{qual_finish}{Likert 1-5 score, includes 99 as error/empty}
#'   \item{qual_defects}{Likert 1-5 score}
#'   \item{qual_ux}{Likert 1-5 user experience score}
#'   \item{del_time_avg}{Average delivery time in days}
#'   \item{del_reliability}{Reliability percentage}
#'   \item{del_tracking}{Tracking availability (0 or 10)}
#'   \item{sus_co2}{Likert 1-7 CO2 score}
#'   \item{sus_waste}{Likert 1-7 waste management score}
#'   \item{sus_material}{Likert 1-7 material score, includes NAs}
#'   \item{sus_social}{Likert 1-7 social impact score}
#' }
#' @usage data(mcdm_raw_data)
#' @name mcdm_raw_data
NULL
