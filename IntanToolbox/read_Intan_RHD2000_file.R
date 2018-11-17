
# Info --------------------------------------------------------------------

# Prof. Adriano O. Andrade, PhD
# adriano@ufu.br
# 09 Nov 2018
# version 1.0

# Libraries ---------------------------------------------------------------


#install.packages if they are not present
if (!require(mmap)) install.packages('mmap')
if (!require(pracma)) install.packages('pracma')
if (!require(dygraphs)) install.packages('dygraphs')
if (!require(bitops)) install.packages('bitops')
if (!require(signal)) install.packages('signal')
if (!require(stringr)) install.packages('stringr')
if (!require(openxlsx)) install.packages('openxlsx')
if (!require(tools)) install.packages('tools')
if (!require(seewave)) install.packages('seewave')
if (!require(fftw)) install.packages('fftw')
if (!require(psd)) install.packages('psd') 
if (!require(EMD)) install.packages('EMD') 


# load libraries
library(mmap)
library(pracma)
library(dygraphs)
library(bitops)
library(signal)
library(stringr)
library(openxlsx)
library(tools)
library(seewave)
library(fftw)
library(psd)
library(EMD)


# File conversion functions ----------------------------------------------------------------------

#' Convert an rdh file to excel. The function will convert all files in the folder specified by filename
#'
#' @param filename ...: use filename <- file.choose() # Seleção do arquivo
#'
#' @return
#' @export
#'
#' @examples
ConvertRHD2Excel <- function(filename)
{
  
  f <- sort(list.files(path=dirname(filename), full.names=TRUE, pattern="*.rhd"))
  
  aa <- substring(basename(f),1,6)
  g <- levels(factor(aa))
  
  
  readFiles <- function(g,f){
    f[str_detect(f, g)]
  }
  
  
  rl <- lapply(as.list(g), readFiles, f)
  
  Ngroups <- length(rl)
  
  for (i in 1:Ngroups)
  {
    Nj <- length(rl[[i]])
    
    tt <- ones(Nj,2) * NA # matrix for storing the beginning and the end of time
    
    for (j in 1:Nj){
      
      X <- OpenIntanFile(rl[[i]][j])
      
      XX <- data.frame(time = X$t_amplifier, chan = t(X$amplifier_data), Pulse = X$board_dig_in_data)
      
      tt[j,1] <- XX$time[1]
      tt[j,2] <- XX$time[length(XX$time)]
      print(tt)
      
      if(tt[j, 1] == 0)
      {
        worksheetName <- basename(rl[[i]][j])
        df_for_workbook <- XX
        out_xlsx <-  paste(dirname(rl[[i]][j]), '/', 
                           file_path_sans_ext(basename(rl[[i]][j])) ,".xlsx", sep="")
        
        WriteDF2ExcelFile(out_xlsx, df_for_workbook, worksheetName)
      }
      else{
        
        WriteDF2ExcelFile(out_xlsx, df_for_workbook = XX, worksheetName)
      }
      
    }
    
  }
  
}


#' Write a dataframe object to an Excel file
#'
#'
#' @param out_xlsx .........: Excel file name, including the path and file extension
#' @param df_for_workbook ..: Dataframe to be written to the Excel file
#' @param worksheetName   ..: Excel worsheet name
#'
#' @return
#' @export
#'
#' @examples
WriteDF2ExcelFile <- function(out_xlsx, df_for_workbook, worksheetName)
{
  # Check to see if file doesn't exist
  if (!file.exists(out_xlsx))  {
    # Create workbook using openxlsx
    wb <- createWorkbook()
    # Add worksheet
    addWorksheet(wb, worksheetName)
    # Write data frame to new worksheet
    writeData(wb, worksheetName, df_for_workbook)
    # Save file
    saveWorkbook(wb, file = out_xlsx)
  } else {
    # Read in existing data
    old_wb <- readWorkbook(out_xlsx,
                           sheet = worksheetName,
                           detectDates = TRUE)
    # Append new data
    new_data <-
      rbind(old_wb,
            df_for_workbook)
    # Load and write updated data frame to existing worksheet
    wb <-  loadWorkbook(out_xlsx)
    writeData(wb, worksheetName, new_data)
    # Save file
    saveWorkbook(wb, out_xlsx, overwrite = TRUE)
  }
}



# User defined functions --------------------------------------------------

isOpen <- function (con, rw = "") { 
  rw <- pmatch(rw, c("read", "write"), 0) 
  res <- FALSE 
  tryCatch({ 
    res <- .Internal(isOpen(con, rw)) 
  }, error = function(ex) { 
  }) 
  res 
} 


# Text fields are not stored as null-terminated strings as is common in the C family of languages. Rather, they are stored as lengthprefixed
# strings using the QString style from the open-source Qt framework for C++. In the QString format, each string begins
# with a 32-bit unsigned number (uint32) that indicates the length of the string, in bytes. If this number equals 0xFFFFFFFF, the
# string is null. A series of 16-bit (2-byte) Unicode characters follows, and there is no special character to indicate the end of the
# string.

fread_QString <- function (fid)
{
  
  a <- ' '
  
  length <- readBin(con=fid, what = "integer" , n =  1, size=4, signed = FALSE) # uint32
  
  try(
    if(toString(as.hexmode(length)) == "ffffffff"){
      return("")
    }
  )
  
  
  if(length == 0){
    return(a)
  }
  else{
    
    length <- length / 2; # convert length from bytes to 16-bit Unicode words
    
    a <- character(length)
    
    for (i in 1:length)
    {
      a[i] <- as.character(readBin(con=fid, what = "character" , n =  1, signed = FALSE, size = 2))  # uint16
      
    }
    
    return(paste(a, collapse = ""))
    
  }
  
}


# % s = plural(n)
# % 
# % Utility function to optionally plurailze words based on the value
# % of n.
plural <- function(n)
{
  if (n == 1)
    s <-  ''
  else
    s <- 's'
  
  
  return (s)
}


fread <- function(fid,M=1, N=1, what_="int", size_ =2, signed_ =FALSE)
{
  a <- pracma::zeros(M,N)
  
  for(j in 1:N){
    for(i in 1:M){
      
      a[i,j] <- readBin(con=fid, what = what_ , n = 1, size = size_, signed = signed_)
    }
    
  }
  
  return(t(a))
}



# function out = notch_filter(in, fSample, fNotch, Bandwidth)
# 
# % out = notch_filter(in, fSample, fNotch, Bandwidth)
# %
# % Implements a notch filter (e.g., for 50 or 60 Hz) on vector 'in'.
# % fSample = sample rate of data (in Hz or Samples/sec)
# % fNotch = filter notch frequency (in Hz)
# % Bandwidth = notch 3-dB bandwidth (in Hz).  A bandwidth of 10 Hz is
# %   recommended for 50 or 60 Hz notch filters; narrower bandwidths lead to
# %   poor time-domain properties with an extended ringing response to
# %   transient disturbances.
# %
# % Example:  If neural data was sampled at 30 kSamples/sec
# % and you wish to implement a 60 Hz notch filter:
#   %
notch_filter <- function(xin, fSample, fNotch = 60, Bandwidth = 10)
{
  
  tstep <- 1/fSample
  Fc <- fNotch*tstep
  
  L <- length(xin)
  
  #% Calculate IIR filter parameters
  d <- exp(-2*pi*(Bandwidth/2)*tstep)
  b <- (1 + d*d)*cos(2*pi*Fc)
  a0 <- 1
  a1 <- -b
  a2 <- d*d
  a <- (1 + d*d)/2
  b0 <- 1
  b1 <- -2*cos(2*pi*Fc)
  b2 <- 1
  
  out <- pracma::zeros(1,length(xin))
  out[1] <- xin[1]
  out[2] <- xin[2]
  #% (If filtering a continuous data stream, change out(1) and out(2) to the
  #  %  previous final two values of out.)
  
  #% Run filter
  for (i in 3:L)
  {
    out[i] <- (a*b2*xin[i-2] + a*b1*xin[i-1] + a*b0*xin[i] - a2*out[i-2] - a1*out[(i-1)])/a0
    
  }
  
  return(out)
}



# User defined structures -------------------------------------------------


setClass(
  "Notes",
  slots = list(
    note1 = "character",
    note2 = "character",
    note3 = "character"
  )
)

setClass(
  "FreParam",
  slots = list(
    amplifier_sample_rate = "numeric",
    aux_input_sample_rate = "numeric",
    supply_voltage_sample_rate = "numeric",
    board_adc_sample_rate = "numeric",
    board_dig_in_sample_rate = "numeric",
    desired_dsp_cutoff_frequency = "numeric",
    actual_dsp_cutoff_frequency = "numeric",
    dsp_enabled = "numeric",
    desired_lower_bandwidth = "numeric",
    actual_lower_bandwidth = "numeric",
    desired_upper_bandwidth = "numeric",
    actual_upper_bandwidth = "numeric",
    notch_filter_frequency = "numeric",
    desired_impedance_test_frequency = "numeric",
    actual_impedance_test_frequency = "numeric"
  )
)


setClass(
  "SpikeTriggerStruct",
  slots = list(
    voltage_trigger_mode = "integer",
    voltage_threshold = "integer",
    digital_trigger_channel = "integer",
    digital_edge_polarity = "integer"
  )
)

setClass(
  "ChannelStruct",
  slots = list(
    native_channel_name = "character",
    custom_channel_name = "character",
    native_order = "integer",
    custom_order = "integer",
    board_stream = "integer",
    chip_channel = "integer",
    port_name = "character",
    port_prefix = "character",
    port_number = "integer",
    electrode_impedance_magnitude = "numeric",
    electrode_impedance_phase = "numeric"
  )
)

# Program -----------------------------------------------------------------
OpenIntanFile <- function(filename)
{
  fid <- file(filename, "rb") #conexão
  
  if(isOpen(fid, rw="") == TRUE)
  {
    filesize <- file.size(filename) # tamanho do arquivo em bytes
    
    # Check 'magic number' at beginning of file to make sure this is an Intan
    # Technologies RHD2000 data file.
    magic_number <- readBin(con=fid, what = "int" , n =  1, signed = FALSE) 
    
    try(
      if(toString(as.hexmode(magic_number)) != "c6912702"){
        stop("Unrecognized file type.")
      }
    )
    
    # Read version number.
    data_file_main_version_number <- readBin(con=fid, what = "int" , n =  1, size = 2) 
    data_file_secondary_version_number <- readBin(con=fid, what = "int" , n =  1, size = 2) 
    
    fprintf('\n')
    fprintf("Reading Intan Technologies RHD2000 Data File, Version %s.%s\n", data_file_main_version_number, data_file_secondary_version_number)
    
    if (data_file_main_version_number == 1){
      num_samples_per_data_block <- 60
    }
    else{
      num_samples_per_data_block <- 128
    }
    
    
    # Read information of sampling rate and amplifier frequency settings.
    sample_rate <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
    dsp_enabled <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16'
    
    actual_dsp_cutoff_frequency <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
    actual_lower_bandwidth <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
    actual_upper_bandwidth <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
    
    desired_dsp_cutoff_frequency <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
    desired_lower_bandwidth <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
    desired_upper_bandwidth <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
    
    # This tells us if a software 50/60 Hz notch filter was enabled during
    # the data acquisition.
    
    notch_filter_mode <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16'
    notch_filter_frequency <- 0;
    
    if (notch_filter_mode == 1){
      notch_filter_frequency = 50
    }   else if (notch_filter_mode == 2){
      notch_filter_frequency = 60
    }
    
    
    desired_impedance_test_frequency <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
    actual_impedance_test_frequency <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
    
    ## Place notes in data strucure
    notes <- new("Notes", 
                 note1 = fread_QString(fid),
                 note2 = fread_QString(fid),
                 note3 = fread_QString(fid))
    
    #If data file is from GUI v1.1 or later, see if temperature sensor data
    # was saved.
    num_temp_sensor_channels <- 0
    if ((data_file_main_version_number == 1 && data_file_secondary_version_number >= 1) 
        || (data_file_main_version_number > 1)){
      num_temp_sensor_channels <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16'
    }
    
    
    # If data file is from GUI v1.3 or later, load eval board mode.
    eval_board_mode <- 0
    if ((data_file_main_version_number == 1 && data_file_secondary_version_number >= 3) 
        || (data_file_main_version_number > 1)) {
      eval_board_mode <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
    }
    
    
    # If data file is from v2.0 or later (Intan Recording Controller),
    #load name of digital reference channel.
    if (data_file_main_version_number > 1){
      reference_channel <- fread_QString(fid)
    }
    
    
    # Place frequency-related information in data structure.
    
    frequency_parameters <- new( "FreParam",
                                 amplifier_sample_rate = sample_rate,
                                 aux_input_sample_rate = sample_rate / 4,
                                 supply_voltage_sample_rate = sample_rate / num_samples_per_data_block,
                                 board_adc_sample_rate = sample_rate,
                                 board_dig_in_sample_rate = sample_rate,
                                 desired_dsp_cutoff_frequency = desired_dsp_cutoff_frequency,
                                 actual_dsp_cutoff_frequency = actual_dsp_cutoff_frequency,
                                 dsp_enabled = dsp_enabled,
                                 desired_lower_bandwidth = desired_lower_bandwidth,
                                 actual_lower_bandwidth = actual_lower_bandwidth,
                                 desired_upper_bandwidth = desired_upper_bandwidth,
                                 actual_upper_bandwidth = actual_upper_bandwidth,
                                 notch_filter_frequency = notch_filter_frequency,
                                 desired_impedance_test_frequency = desired_impedance_test_frequency,
                                 actual_impedance_test_frequency = actual_impedance_test_frequency
    )
    
    # Define data structure for spike trigger settings.
    new_trigger_channel <- list(new( "SpikeTriggerStruct"))
    spike_triggers <- list(new( "SpikeTriggerStruct"))
    
    
    # Define data structure for data channels.
    new_channel <- list(new( "ChannelStruct"))
    
    # Create structure arrays for each type of data channel.
    
    amplifier_channels <- list(new( "ChannelStruct"))
    aux_input_channels <- list(new( "ChannelStruct"))
    supply_voltage_channels <- list(new( "ChannelStruct"))
    board_adc_channels <- list(new( "ChannelStruct"))
    board_dig_in_channels <- list(new( "ChannelStruct"))
    board_dig_out_channels <- list(new( "ChannelStruct"))
    
    amplifier_index <- 1;
    aux_input_index <- 1;
    supply_voltage_index <- 1;
    board_adc_index <- 1;
    board_dig_in_index <- 1;
    board_dig_out_index <- 1;
    
    # Read signal summary from data file header.
    
    number_of_signal_groups <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
    
    
    for (signal_group in 1:number_of_signal_groups)
    {
      signal_group_name <- fread_QString(fid);
      signal_group_prefix <- fread_QString(fid);
      signal_group_enabled <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
      signal_group_num_channels <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
      signal_group_num_amp_channels <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16   
      
      if (signal_group_num_channels > 0 && signal_group_enabled > 0)
      {
        new_channel[[1]]@port_name <- signal_group_name
        new_channel[[1]]@port_prefix <- signal_group_prefix
        new_channel[[1]]@port_number <- signal_group
        
        for (signal_channel in 1:signal_group_num_channels)
        {
          
          new_channel[[1]]@native_channel_name <- fread_QString(fid)
          new_channel[[1]]@custom_channel_name <- fread_QString(fid)
          new_channel[[1]]@native_order <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
          new_channel[[1]]@custom_order <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
          signal_type <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
          channel_enabled <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
          new_channel[[1]]@chip_channel <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
          new_channel[[1]]@board_stream <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
          new_trigger_channel[[1]]@voltage_trigger_mode <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
          new_trigger_channel[[1]]@voltage_threshold <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
          new_trigger_channel[[1]]@digital_trigger_channel <- readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
          new_trigger_channel[[1]]@digital_edge_polarity <-readBin(con=fid, what = "int" , n =  1, size = 2) #'int16
          new_channel[[1]]@electrode_impedance_magnitude <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
          new_channel[[1]]@electrode_impedance_phase <- readBin(con=fid, what = "numeric" , n =  1, size = 4) #'single'
          
          if (channel_enabled)
          {
            if(signal_type == 0)
            {
              amplifier_channels [[amplifier_index]] <- new_channel
              spike_triggers[[amplifier_index]] <- new_trigger_channel
              amplifier_index <- amplifier_index + 1
            } else if (signal_type == 1)
            {
              aux_input_channels [[aux_input_index]] <-  new_channel
              aux_input_index <- aux_input_index + 1
            } else if (signal_type == 2)
            {
              supply_voltage_channels [[supply_voltage_index]] <-  new_channel
              supply_voltage_index <- supply_voltage_index + 1
            } else if (signal_type == 3)
            {
              board_adc_channels [[board_adc_index]] <-  new_channel
              board_adc_index <- board_adc_index + 1
            } else if (signal_type == 4)
            {
              board_dig_in_channels[[board_dig_in_index]] <- new_channel
              board_dig_in_index <- board_dig_in_index + 1
            } else if (signal_type == 5)
            {
              board_dig_out_channels[[board_dig_out_index]] <- new_channel
              board_dig_out_index <- board_dig_out_index + 1
            } else
            {
              message('Unknown channel type')
            }
            
            
          }
        }
        
      }
    }
    
    
    # Summarize contents of data file.
    num_amplifier_channels <- amplifier_index - 1
    num_aux_input_channels <- aux_input_index - 1
    num_supply_voltage_channels <- supply_voltage_index - 1
    num_board_adc_channels <- board_adc_index - 1
    num_board_dig_in_channels <- board_dig_in_index - 1
    num_board_dig_out_channels <- board_dig_out_index - 1
    
    fprintf('Found %d amplifier channel%s.\n',
            num_amplifier_channels, plural(num_amplifier_channels))
    fprintf('Found %d auxiliary input channel%s.\n', 
            num_aux_input_channels, plural(num_aux_input_channels))
    fprintf('Found %d supply voltage channel%s.\n', 
            num_supply_voltage_channels, plural(num_supply_voltage_channels))
    fprintf('Found %d board ADC channel%s.\n',
            num_board_adc_channels, plural(num_board_adc_channels))
    fprintf('Found %d board digital input channel%s.\n', 
            num_board_dig_in_channels, plural(num_board_dig_in_channels))
    fprintf('Found %d board digital output channel%s.\n', 
            num_board_dig_out_channels, plural(num_board_dig_out_channels))
    fprintf('Found %d temperature sensor channel%s.\n', 
            num_temp_sensor_channels, plural(num_temp_sensor_channels))
    fprintf('\n')
    
    
    # Determine how many samples the data file contains.
    
    # Each data block contains num_samples_per_data_block amplifier samples.
    bytes_per_block <- num_samples_per_data_block * 4;  # timestamp data
    bytes_per_block <- bytes_per_block + num_samples_per_data_block * 2 * num_amplifier_channels
    # Auxiliary inputs are sampled 4x slower than amplifiers
    bytes_per_block <- bytes_per_block + (num_samples_per_data_block / 4) * 2 * num_aux_input_channels
    # Supply voltage is sampled once per data block
    bytes_per_block <- bytes_per_block + 1 * 2 * num_supply_voltage_channels
    # Board analog inputs are sampled at same rate as amplifiers
    bytes_per_block <- bytes_per_block + num_samples_per_data_block * 2 * num_board_adc_channels
    # Board digital inputs are sampled at same rate as amplifiers
    
    if (num_board_dig_in_channels > 0){
      bytes_per_block <- bytes_per_block + num_samples_per_data_block * 2
    }
    #Board digital outputs are sampled at same rate as amplifiers
    if (num_board_dig_out_channels > 0){
      bytes_per_block = bytes_per_block + num_samples_per_data_block * 2
    }
    #Temp sensor is sampled once per data block
    if (num_temp_sensor_channels > 0){
      bytes_per_block = bytes_per_block + 1 * 2 * num_temp_sensor_channels 
    }
    
    
    ## How many data blocks remain in this file?
    data_present <- 0
    bytes_remaining <- filesize - seek(con=fid)
    if (bytes_remaining > 0){
      data_present <- 1
    }
    
    num_data_blocks <- bytes_remaining / bytes_per_block
    
    num_amplifier_samples <- num_samples_per_data_block * num_data_blocks
    num_aux_input_samples <- (num_samples_per_data_block / 4) * num_data_blocks
    num_supply_voltage_samples <- 1 * num_data_blocks
    num_board_adc_samples <- num_samples_per_data_block * num_data_blocks
    num_board_dig_in_samples <- num_samples_per_data_block * num_data_blocks
    num_board_dig_out_samples <- num_samples_per_data_block * num_data_blocks
    
    record_time <- num_amplifier_samples / sample_rate
    
    
    if (data_present){
      fprintf('File contains %0.3f seconds of data.  Amplifiers were sampled at %0.2f kS/s.\n', 
              record_time, sample_rate / 1000)
      fprintf('\n')
    }
    else
    {
      fprintf('Header file contains no data.  Amplifiers were sampled at %0.2f kS/s.\n',
              sample_rate / 1000);
      fprintf('\n')
    }
    
    if (data_present)
    {
      
      # Pre-allocate memory for data.
      fprintf('Allocating memory for data...\n')
      
      t_amplifier <- pracma::zeros(1, num_amplifier_samples)
      
      amplifier_data <- pracma::zeros(num_amplifier_channels, num_amplifier_samples)
      aux_input_data <- pracma::zeros(num_aux_input_channels, num_aux_input_samples)
      supply_voltage_data <- pracma::zeros(num_supply_voltage_channels, num_supply_voltage_samples)
      temp_sensor_data <- pracma::zeros(num_temp_sensor_channels, num_supply_voltage_samples)
      board_adc_data <- pracma::zeros(num_board_adc_channels, num_board_adc_samples)
      board_dig_in_data <- pracma::zeros(num_board_dig_in_channels, num_board_dig_in_samples)
      board_dig_in_raw <- pracma::zeros(1, num_board_dig_in_samples)
      board_dig_out_data <- pracma::zeros(num_board_dig_out_channels, num_board_dig_out_samples)
      board_dig_out_raw <- pracma::zeros(1, num_board_dig_out_samples)
      
      #Read sampled data from file.
      fprintf('Reading data from file...\n')
      
      amplifier_index <- 1
      aux_input_index <-1
      supply_voltage_index <- 1
      board_adc_index <- 1
      board_dig_in_index <- 1
      board_dig_out_index <- 1
      
      print_increment <- 10
      percent_done <- print_increment
      
      for (i in 1:num_data_blocks)
      {
        
        if ((data_file_main_version_number == 1 && data_file_secondary_version_number >= 2)
            || (data_file_main_version_number > 1))
          t_amplifier[1, amplifier_index:(amplifier_index + num_samples_per_data_block - 1)] <- readBin(con=fid, what = "int" , n =  num_samples_per_data_block, size = 4) #'int32'
        else
          t_amplifier[1, amplifier_index:(amplifier_index + num_samples_per_data_block - 1)] <- readBin(con=fid, what = "int" , n =  num_samples_per_data_block, size = 4, signed = FALSE) #'uint32'
        
        
        
        if (num_amplifier_channels > 0){
          amplifier_data[ , amplifier_index:(amplifier_index + num_samples_per_data_block - 1)] <- fread(fid,M=num_samples_per_data_block, N=num_amplifier_channels)
          
        }
        
        
        if (num_aux_input_channels > 0)
        {
          aux_input_data[ , aux_input_index:(aux_input_index + (num_samples_per_data_block / 4) - 1)] <- fread (fid, M= num_samples_per_data_block / 4 ,N= num_aux_input_channels)
        }
        
        
        if (num_supply_voltage_channels > 0){
          supply_voltage_data[ , supply_voltage_index] <- fread (fid, M= 1 ,N= num_supply_voltage_channels)
        }
        
        
        if (num_temp_sensor_channels > 0){
          temp_sensor_data[ , supply_voltage_index] <-  fread(fid, M=1, N=num_temp_sensor_channels, signed_ =TRUE); #int16
        }
        
        
        if (num_board_adc_channels > 0){
          board_adc_data[ , board_adc_index:(board_adc_index + num_samples_per_data_block - 1)] <- fread(fid, M=num_samples_per_data_block, N=num_board_adc_channels);#'uint16'
        }
        
        if (num_board_dig_in_channels > 0){
          board_dig_in_raw[board_dig_in_index:(board_dig_in_index + num_samples_per_data_block - 1)]   <- readBin(con=fid, what = "int" , n =  num_samples_per_data_block, size = 2, signed = FALSE) #'uint16'
        }
        
        if (num_board_dig_out_channels > 0){
          board_dig_out_raw[board_dig_out_index:(board_dig_out_index + num_samples_per_data_block - 1)] <- readBin(con=fid, what = "int" , n =  num_samples_per_data_block, size = 2, signed = FALSE) #'uint16'
        }
        
        
        amplifier_index <- amplifier_index + num_samples_per_data_block
        aux_input_index <- aux_input_index + (num_samples_per_data_block / 4)
        supply_voltage_index <- supply_voltage_index + 1
        board_adc_index <- board_adc_index + num_samples_per_data_block
        board_dig_in_index <- board_dig_in_index + num_samples_per_data_block
        board_dig_out_index <- board_dig_out_index + num_samples_per_data_block
        
        fraction_done <- 100 * (i / num_data_blocks)
        if (fraction_done >= percent_done)
        {
          fprintf('%d%% done...\n', percent_done)
          percent_done <- percent_done + print_increment
        }
        
      }
      
    }
    
    ## Make sure we have read exactly the right amount of data.
    bytes_remaining = filesize - seek(fid);
    if (bytes_remaining != 0){
      message('Error: End of file not reached.')
    }
    
    close(fid) #fecha a conexão
    
    if (data_present)
    {
      fprintf('Parsing data...\n')
      
      # % Extract digital input channels to separate variables.
      if(num_board_dig_in_channels >0 )
      {
        for (i in 1:num_board_dig_in_channels)
        {
          mask <- 2^(board_dig_in_channels[[i]][[i]]@native_order) * pracma::ones(1, length(board_dig_in_raw)) 
          board_dig_in_data[i, ] <- (bitAnd(board_dig_in_raw, mask) > 0)
        }
      }
      if(num_board_dig_out_channels>0)
      {
        for (i in 1:num_board_dig_out_channels)
        {
          mask <- 2^(board_dig_out_channels[[i]][[i]]@native_order) * ones(1, length(board_dig_out_raw))
          board_dig_out_data[i, ] <- (bitAnd(board_dig_out_raw, mask) > 0)
        }
      }
      
      
      
      # Scale voltage levels appropriately.
      amplifier_data <- 0.195 * (amplifier_data - 32768) # units = microvolts
      aux_input_data <- 37.4e-6 * aux_input_data # units = volts
      supply_voltage_data <- 74.8e-6 * supply_voltage_data # units = volts
      
      if (eval_board_mode == 1){
        board_adc_data <- 152.59e-6 * (board_adc_data - 32768) # units = volts
      }
      else if (eval_board_mode == 13){ # Intan Recording Controller
        board_adc_data <- 312.5e-6 * (board_adc_data - 32768) # units = volts  
      }
      else{
        board_adc_data <- 50.354e-6 * board_adc_data # units = volts
      }
      temp_sensor_data <- temp_sensor_data / 100 # units = deg C
      
      # Check for gaps in timestamps.
      num_gaps = sum(diff(t_amplifier) != 1)
      if (num_gaps == 0){
        fprintf('No missing timestamps in data.\n')
      }
      else{
        fprintf('Warning: %d gaps in timestamp data found.  Time scale will not be uniform!\n', num_gaps)
      }
      
      
      #Scale time steps (units = seconds).
      t_amplifier <- t_amplifier / sample_rate
      t_aux_input <- t_amplifier[seq(1,length(t_amplifier), by = 4)]
      t_supply_voltage <- t_amplifier[seq(1,length(t_amplifier), by=num_samples_per_data_block)]
      t_board_adc <- t_amplifier
      t_dig <- t_amplifier
      t_temp_sensor <- t_supply_voltage
      
      
      # % If the software notch filter was selected during the recording, apply the
      # % same notch filter to amplifier data here.
      
      if (notch_filter_frequency > 0)
      {
        fprintf('Applying notch filter...\n')
        print_increment <- 10
        percent_done <- print_increment
        
        for(i in 1:num_amplifier_channels)
        {
          amplifier_data[i,] <- notch_filter(amplifier_data[i,], sample_rate, notch_filter_frequency, 10)
          fraction_done <- 100 * (i / num_amplifier_channels)
          if (fraction_done >= percent_done)
          {
            
            fprintf('%d%% done...\n', percent_done);
            percent_done <- percent_done + print_increment
            
          }
          
        }
        
      }
      
    }
  }
  
  return(list(
              t_amplifier = t_amplifier[1,], 
              amplifier_data =  amplifier_data,
              board_dig_in_data = board_dig_in_data[1,] ))

  
}



