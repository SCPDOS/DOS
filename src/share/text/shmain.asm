;All the main bodies of the share hook calls go here

openShare:           
;Called on file create/open
closeShare:          
;Called on file close
closeCompShare:      
;Close all files for a machine
closeTaskShare:      
;Close all files for a task
closeNameShare:      
;Close file by name
lockFileShare:       
;Lock a file region
unlockFileShare:     
;Unlock file region
checkFileLockShare:  
;Check file region locked
openFileListShare:   
;Get MFT information about file
updateFCBfromSFTShr: 
;UNUSED: Update FCB from the SFT
fstClstOfFCBShare:   
;UNUSED: Get first cluster of FCB
closeDupFileShare:   
;Close duplicate handles for a procedure.
closeNewHdlShare:
;Closes the topmost SFT for a file
updateDirShare:      
;Update dir info across all SFTs for a file. 