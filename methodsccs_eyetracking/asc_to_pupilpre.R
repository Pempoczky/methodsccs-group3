# Load raw .asc data and convert to pupilPre friendly format.
# 
# Joshua Krause (j.krause.1@student.rug.nl)
# 220923


require(PupilPre)
require(eyelinker)
require(plyr)

asc_to_pupilpre <- function(path_to_file,
                            label,
                            recorded_eye,
                            trial_start_msg,
                            id_pattern,
                            trial_end_msg,
                            match = "align") {
  
  # PupilPre's ppl_prep_data function expects a sample report from the eye-tracker.
  # This function brings a .asc file into the same format that ppl_prep_data would
  # return, so all the other pupilPre functions can then be utilized as much as possible.
  
  # Thus this file is in large parts based on code from the ppl_prep_data
  # function available here:
  # https://github.com/cran/PupilPre/blob/master/R/process.R
  
  # Additional code parts were taken from the eyelinker vignette to align
  # blinks and saccades. This vignette can be found here:
  # https://cran.r-project.org/web/packages/eyelinker/vignettes/basics.html
  
  # Required libraries:
  # eyelinker and PupilPre
  
  # Parameters:
  # path_to_file = Path to file as string, e.g., "E:/Folder/SubFolder/filename.asc"
  # label = Some subject identifier, e.g., "sub3"
  # recorded_eye = Recorded eye, e.g., "Left"
  # trial_start_msg = Trial begin regular expression pattern, e.g., "var StartTrial [0-9]+"
  # id_pattern = Part of trial pattern just before numerical trial index, e.g., "StartTrial "
  # trial_end_msg = Trial end pattern, e.g., "var EndTrial [0-9]+"
  # match = How to handle out of sample messages, whether to "add" new rows for
  #         these messages or whether to "align" them with row corresponding to
  #         the next existing sample.
  
  # <----------------------------- Function body ------------------------------>
  
  # First read in .asc file
  asc_dat_raw <- read.asc(path_to_file)
  asc_dat <- asc_dat_raw
  
  # Align blinks and saccades to long format as described in the eyelinker
  # vignette. Basically, the stime and etime variables here contain
  # start and end time stamps for the corresponding events (saccade or blink)
  # Note: the %In% operator is an eyelinker helper and not the typical %in% operator!!
  SAC <- cbind(asc_dat$sacc$stime,asc_dat$sacc$etime)
  asc_dat$raw <- mutate(asc_dat$raw, saccade = time %In% SAC)
  BLINK <- cbind(asc_dat$blinks$stime, asc_dat$blinks$etime)
  asc_dat$raw <- mutate(asc_dat$raw, blink = time %In% BLINK)
  
  # Remove by-products to keep memory footprint small
  rm(BLINK)
  rm(SAC)
  
  # Now align messages into long format. Since reception of messages is
  # not necessarily aligned with the sampling rate we might end up with
  # messages received at a number of milliseconds at which we did
  # not receive a sample. These are either added as individual rows to the data
  # frame or aligned with the row corresponding to the next existing sample time
  # point.
  asc_dat$raw$msg <- NA
  matched_comb <- NULL
  
  if(match == "add") {
    # Get matching messages
    matched <- asc_dat$msg[asc_dat$msg$time %in% asc_dat$raw$time,]
    
    # Get unmatched (received outside of sampling interval)
    unmatched <- asc_dat$msg[!(asc_dat$msg$time %in% asc_dat$raw$time),]
    
    # Some messages might be received at same time point but will still be split into
    # multiple rows in the .asc, so merge them here for both matched and unmatched.
    matched_unq <- ddply(matched,c("time","block"),summarize,
                         text=paste0(text,collapse=","))
    
    unmatched_unq <- ddply(unmatched,c("time","block"),summarize,
                           text=paste0(text,collapse=","))
    
    # Save these so that we can later for analysis drop samples that were added just
    # for messages.
    unmatched_times <- unmatched_unq$time
    
    # Create data-frame for unmatched messages that matches layout of raw data
    unmatched_data <- data.frame(matrix(NA,nrow = nrow(unmatched_unq),ncol = ncol(asc_dat$raw)))
    colnames(unmatched_data) <- colnames(asc_dat$raw)
    unmatched_data[,c("block","time","msg")] <- unmatched_unq[,c("block","time","text")]
    
    # Merge and then sort data again by time.
    asc_dat$raw <- rbind(asc_dat$raw,unmatched_data)
    asc_dat$raw <- asc_dat$raw[order(asc_dat$raw$time),]
    
    # Add matched messages to their corresponding time points.
    asc_dat$raw$msg[asc_dat$raw$time %in% matched$time] <- matched_unq$text
    
    # Create a combined message frame used for trial calculation below.
    matched_comb <- rbind(matched_unq,unmatched_unq)
    matched_comb <- matched_comb[order(matched_comb$time),]
    
    # Remove by-products to keep memory footprint small
    rm(matched_unq)
    rm(unmatched_unq)
    rm(unmatched_data)
    
  } else if(match == "align") {
    # Start by setting time of messages received after the last raw data sample
    # to the time of the last raw data sample.
    asc_dat$msg$time[asc_dat$msg$time > max(asc_dat$raw$time)] <- max(asc_dat$raw$time)
    
    # Now get matching messages
    matched <- asc_dat$msg[asc_dat$msg$time %in% asc_dat$raw$time,]
    
    # Get unmatched (received outside of sampling interval)
    unmatched <- asc_dat$msg[!(asc_dat$msg$time %in% asc_dat$raw$time),]
    
    # adjust unmatched time points
    returnNextTime <- function(t){
      return(asc_dat$raw$time[asc_dat$raw$time > t][1])
    }
    
    unmatched$time <- sapply(unmatched$time, returnNextTime)
    
    if(length(unmatched$time[!(unmatched$time %in% asc_dat$raw$time)])) {
      stop("Problems with aligning messages, try setting the match parameter to 'add'.\n")
      sample_ppl_prepped <- NULL
    }
    
    # Now we write both, the matched and unmatched into the correct row.
    # First combine both matched and previously unmatched messages.
    combined_msgs_aligned <- rbind(matched,unmatched)
    
    # Now we make sure again that per time-point we have only one message
    matched_comb <- ddply(combined_msgs_aligned,c("time","block"),summarize,
                          text=paste0(text,collapse=","))
    
    # Remove by-products to keep memory footprint small
    rm(combined_msgs_aligned)
    rm(returnNextTime)
    
    # Write all messages to raw data
    asc_dat$raw$msg[asc_dat$raw$time %in% matched_comb$time] <- matched_comb$text

  } else {
    stop("Parameter match needs to be either 'add' or 'align'.\n")
  }
  
  # Remove by-products to keep memory footprint small
  rm(matched)
  rm(unmatched)
  
  # Now create trial variable
  # First we get the regular expression matches for each row in the DF
  # for both trial start and trial end messages.
  res_start <- regexpr(trial_start_msg,
                       matched_comb$text)
  
  res_end <- regexpr(trial_end_msg,
                     matched_comb$text)
  
  # Check how many indices we could recover. If these do not match we have
  # a corrupted file.
  n_start <- length(res_start[res_start != -1])
  n_end <- length(res_end[res_end != -1])
  
  if(n_start != n_end) {
    stop("Number of trial start messages does not match number of trial end messages.\n")
  }

  # Get time-points matching start and end messages
  start_indices <- res_start != -1
  trialss <- matched_comb$time[start_indices]
  end_indices <- res_end != -1
  trialse <- matched_comb$time[end_indices]
  
  # Get the identifier of a trial encoded in the asc.
  start_matches <- regmatches(matched_comb$text,res_start)
  start_matches <- regmatches(start_matches,regexpr(paste0(id_pattern,"[0-9]+"),start_matches))
  
  # Just get the total trial number, i.e., the numeric sequence following "id_pattern"
  start_matches <- regmatches(start_matches,regexpr("[0-9]+",start_matches))
  
  # Create again an "interval" that is used to check, for each time point, whether
  # that time-point falls between the start and the end of a trial.
  # see: help(whichInterval)
  # and: https://github.com/cran/eyelinker/blob/master/R/utils.R
  trialsse <- cbind(trialss,trialse)
  
  # Now use the eyelinker helpers to create an in_trial variable
  # and a trial variable.
  # Note: this is again not the typical %in% operator!!
  asc_dat$raw$in_trial <- asc_dat$raw$time %In% trialsse
  asc_dat$raw$trial <- whichInterval(asc_dat$raw$time,trialsse)
  
  # Assign correct trial identifiers.
  asc_dat$raw$trial[asc_dat$raw$in_trial] <- start_matches[asc_dat$raw$trial[asc_dat$raw$in_trial]]
  
  # Now we can get a data-frame with all the columns that ppl_prep_data
  # would expect. This needs to contain all the necessary columns
  # included in the original ppl_prep_data function. Here also two
  # optional ones (EYE_TRACKED and SAMPLE_MESSAGE) are added as well.
  # See: https://github.com/cran/PupilPre/blob/master/R/process.R
  
  # Calculate final dimension of data frame.
  n_samples <- nrow(asc_dat$raw)
  
  data_ppl_pre <- data.frame(
    "RECORDING_SESSION_LABEL"= as.factor(rep(label,length.out=n_samples)),
    "LEFT_PUPIL_SIZE" = asc_dat$raw$ps,
    "LEFT_GAZE_X" = asc_dat$raw$xp,
    "LEFT_GAZE_Y" = asc_dat$raw$yp,
    "LEFT_IN_BLINK" = as.numeric(asc_dat$raw$blink),
    "LEFT_IN_SACCADE" = as.numeric(asc_dat$raw$saccade),
    "LEFT_ACCELERATION_X" = rep(NA,length.out=n_samples),
    "LEFT_ACCELERATION_Y" = rep(NA,length.out=n_samples),
    "LEFT_VELOCITY_X" = rep(NA,length.out=n_samples),
    "LEFT_VELOCITY_Y" = rep(NA,length.out=n_samples),
    "RIGHT_PUPIL_SIZE" = rep(NA,length.out=n_samples),
    "RIGHT_GAZE_X" = rep(NA,length.out=n_samples),
    "RIGHT_GAZE_Y" = rep(NA,length.out=n_samples),
    "RIGHT_IN_BLINK" = rep(NA,length.out=n_samples),
    "RIGHT_IN_SACCADE" = rep(NA,length.out=n_samples),
    "RIGHT_ACCELERATION_X" = rep(NA,length.out=n_samples),
    "RIGHT_ACCELERATION_Y" = rep(NA,length.out=n_samples),
    "RIGHT_VELOCITY_X" = rep(NA,length.out=n_samples),
    "RIGHT_VELOCITY_Y" = rep(NA,length.out=n_samples),
    "TIMESTAMP" = asc_dat$raw$time,
    "TRIAL_INDEX" = asc_dat$raw$trial,
    "EYE_TRACKED" = rep(recorded_eye,length.out=n_samples),
    "SAMPLE_MESSAGE" = asc_dat$raw$msg
  )
  
  # And finally, ppl_prep_data can be called! The RECORDING_SESSION_LABEL
  # column will be renamed to subject. This column should then contain only
  # the label value passed as argument to this function.
  sample_ppl_prepped <- ppl_prep_data(data_ppl_pre, Subject = "RECORDING_SESSION_LABEL")
  
  # Add some useful variables not added by pupilpre by default, such as a
  # logical variable indicating whether a specific data-point is within
  # a trial or two logical variables indicating the beginning or end of a new
  # trial.
  sample_ppl_prepped$in_trial <- asc_dat$raw$in_trial
  sample_ppl_prepped$trial_start <- sample_ppl_prepped$TIMESTAMP %in% trialss
  sample_ppl_prepped$trial_end <- sample_ppl_prepped$TIMESTAMP %in% trialse
  
  # In case the match parameter was set to 'add', include a variable that
  # marks rows that were added to the raw data to include out of sample messages.
  if(match == "add") {
    sample_ppl_prepped$added <- sample_ppl_prepped$TIMESTAMP %in% unmatched_times
  }
  
  return(list("pplpreData" = sample_ppl_prepped,
              "rawMessages" = asc_dat$msg,
              "rawData" = asc_dat_raw))
}
