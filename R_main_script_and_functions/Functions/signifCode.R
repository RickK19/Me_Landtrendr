signifCode <- function(x)
{
  return(
    dplyr::case_when(x >= 0.05 & x < 0.1 ~ ".",
                     x >= 0.01  & x < 0.05 ~ "*",
                     x >= 0.001  & x < 0.01 ~ "**",
                     x < 0.001 ~ "***",
                     TRUE ~ "")
  )
}