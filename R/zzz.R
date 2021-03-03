.onAttach <- function(lib,pkg) {
  packageStartupMessage(sprintf("\nbifrost %s (%s)",packageDescription("bifrost")$Version,
                                packageDescription("bifrost")$Date), "\n",
                        "------------------------------------------------------------------------------
* Bifrost developers: Sondre Hølleland and Samuel Subbey and others
------------------------------------------------------------------------------")
  ggplot2::theme_set(ggplot2::theme_bw())
}
