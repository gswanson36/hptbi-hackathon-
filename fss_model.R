################################################################################
#
################################################################################
# FSS Model
# v1.0
# Args:
#   data a data.frame resulting from a call to prepare_fss_data()
#
# Return:
#   An R object.  This object will have the "hackathon_fss_model" class
#   prepended to it such that a call to predict can be used to generate
#   predictions from the training and testing data sets.
#

fss_model <- function(data) {

  ##############################################################################
  # User code starts here
  rtn <- lm(formula = fss_total ~ gcseyeed + gcsseded + ctskullfrac + 
              ctintraventhem + gcsmotoricu + gcseyeobicu + admittoicuadmit2 + 
              admittoext + admittoicpend1 + admittoicpstart3 + 
              newgastyn + decomcranyn + admittolmbdrain + 
              hosplos, 
            data = data)

  # User code ends here
  ##############################################################################

  class(rtn) <- c("hackathon_fss_model", class(rtn))
  rtn
}

################################################################################
# Predict Hackathon Mortality Model
#
# An S3 function call for hackathon_fss_model
#
# Args:
#   object  a hackathon_fss_model object
#   newdata a data.frame
#   ...     additional arguments passed through.  Not expected to be used as
#           part of the hackathon.
#
# Return:
#   A numeric vector of length equal to the nrow(newdata) representing the
#   predicted total FSS score.
#

predict.hackathon_fss_model <- function(object, newdata, ...) {

  ##############################################################################
  # user defined code starts here

  rtn <- as.integer(stats::predict.lm(object, newdata, type = "response"))
  
  rtn[rtn <  6L] <- 6L
  rtn[rtn > 30L] <- 30L
  rtn
}
 
################################################################################
#                                 End of File
################################################################################
