TODO:
  
* IAT flags
* general flagging methods
  
Add in the flagging functions to noggin.

Need to return a df which reports subject number, flag1, flag2, etc. and "any flag" column as an OR across all the flags (for any flag) or users can decide on their on combination of flags.

Each scoring method first needs to transform the data into a "standard" dataframe.  This dataframe turns any platform into a standard df for later processing.  This same df can be used to process flags, etc.
