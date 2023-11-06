# SCPDOS
Some notes on documentation

## Various interfaces:
Int 41h = DOS API (DAPI)

Int 4Fh AX=11xxh = DOS Redirector Interface (DRI)

Int 4Fh AX=12xxh = Internal DOS Interface (IDI)

## API function states
- Proposed (A proposed new API function)
- Accepted (A accepted but not yet implemented API function)
- Preliminary (Documented but subject to change, such as the IDI, DRI functions and Multitasking hooks as these are more or less undocumented in the DOS days)
- Current (Standard DOS API functions. Will be supported. If ever depreciated, then they return an error code and their function numbers are NOT recycled)
- Obsolete (Use an alternative function for everything EXCEPT ensuring compatibility. The FCB functions fall here)
- Removed (This function henceforth will only ever return an error)

## NOTES:
- Driver loads (initial kernel drivers and CONFIG.SYS loads) allow for DOS CHAR functions, version check and get/set date/time!
- Kernel drivers should have only one type of each driver in the chain but we can handle more! At the very start DOS will scan this chain and set the 
first drivers with the CLOCK$ and CON bits set to take control of the devices BEFORE init. Therefore an implementer MUST have these two drivers be the first
two drivers in the chain, if ANY of the other drivers are going to use CHAR functions or get/set date/time! More detail in the OEM init guide.
-When calling an Int 41h function, if the function returns with a success (CF=NC), then unless the function specifies a meaning to (e)ax, the register (e)ax is to be considered as trashed. In many cases it will be zeroed partially (just al), if not the whole of (e)ax. This is not the case if the function returns an error (CF=CY), where the error code for the request will be placed in ax.
