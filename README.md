# SCPDOS

## NOTES:
-When calling an Int 21h function, if the function returns with a success (CF=NC), then unless the function specifies a meaning to (r)ax, the register (r)ax is to be considered as trashed. In many cases it will be zeroed partially (just al), if not the whole of (r)ax. This is not the case if the function returns an error (CF=CY), where the error code for the request will be placed in ax.

## Default localisation information
-DOS is by default in CP437 with a COUNTRY setting of 044 (GB). 
That means console is as per default hardware driver for the console, but time, date, currency, data separation etc characters and layouts are all per GB localisation. 

-If using legacy SCP/BIOS, then the hardware keyboard is a UK Enhanced PC 105-key keyboard layout. The hardware output charset is CP437 compatible.

## Join note
Join currently depends on the fact that you can only mount a local drive onto a
local drive. Though there are possible combinations where one may use a subst/net
through a join host and a join client, this is forbidden in this version. A join host must be a local drive only. A join drive must a local drive only.

## CONFIG.SYS notes
Valid terminator characters for commands on a CONFIG.SYS lines are space, comma, semicolon, equal sign and tab. These correspond to the COMMAND.COM valid argument separators which are a subset to the FCB filename separators. 
Thus, the terminators are a superset of the FCB filename (which is desirable?).
