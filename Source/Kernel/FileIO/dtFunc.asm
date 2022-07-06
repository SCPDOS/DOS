;Major kernel date time functions and subroutines

getDate:           ;ah = 2Ah
    call readDateTimeRecord ;Update date if necessary
    call getUserRegs
    mov dx, word [dayOfMonth]   ;Read as a word to get monthOfYear in dh
;    mov dh, byte [monthOfYear]
    movzx ecx, byte [years]
    mov al, byte [dayOfWeek]
    mov word [rsi + callerFrame.rdx], dx
    mov word [rsi + callerFrame.rcx], cx
    return  ;al is returned as error code
setDate:           ;ah = 2Bh
;	CX = year (1980-2099)
;   DH = month (1-12)
;	DL = day (1-31)
    call writeDate  ;Set ax to 0 or -1 depending
    return
getTime:           ;ah = 2Ch
    call readDateTimeRecord ;Update date if necessary, time in CLOCKrecrd
    call getUserRegs
    mov cx, word [CLOCKrecrd + clkStruc.hours]
    mov dx, word [CLOCKrecrd + clkStruc.hseconds]
    mov word [rsi + callerFrame.rdx], dx
    mov word [rsi + callerFrame.rcx], cx
    return  ;al is returned as error code
setTime:           ;ah = 2Dh
;   CH = hour (0-23)
;	CL = minutes (0-59)
;	DH = seconds (0-59)
;	DL = hundredths (0-99)
    cmp ch, 23
    ja .exitBad
    cmp cl, 59
    ja .exitBad
    cmp dh, 59
    ja .exitBad
    cmp dl, 99
    ja .exitBad
;Now we read date (updating internal count if necessary)
    call readDateTimeRecord
    lea rbx, CLOCKrecrd ;Read into clock record
    movzx eax, word [daysOffset]    ;Get day offset into eax
    mov word [rbx + clkStruc.dateWord], ax  ;Write CLOCK Record
    mov word [rbx + clkStruc.minutes], cx
    mov word [rbx + clkStruc.hseconds], dx
    mov ecx, 6
    xor rbp, rbp    ;Tell it we are a chardev
    call primReqWriteSetup   ;rbx now points to request header
    mov rsi, qword [clockPtr]   ;Get clock driver pointer
    call goDriver
    xor al, al
    return
.exitBad:
    mov al, -1
    return
;------------------------
;   Utility functions   :
;------------------------
writeDate:
    cmp cx, 120
    jae .exitBad
    cmp dh, 12
    ja .exitBad
    cmp dl, 31
    ja .exitBad
    mov word [dayOfMonth], dx   ;Write as a word
    ;mov byte [monthOfYear], dh
    mov byte [years], cl    ;Save the years count
    call setDaysInFeb   ;Set days in february this year
    mov ch, cl ;Get years count in ch
    and cl, 4   ;Get the number of years to the current 4 year bunch
    and ch, 3   ;Get the offset into the current 4 year bunch
    movzx eax, cl ;Zero extend to eax
    mov ebx, 366+3*365  ;Move number of days in 4 year bunch to ebx
    mul ebx ;Multiply number of years in 4 year bunch with days in 4 year bunch
    ;eax has the number of days from 01/01/1980 to 01/01/start of 4 year bunch
    movzx ecx, ch ;Zero extend ch to ecx
.addYears:
    jecxz .addLeap
    add eax, 365    ;Add the days in the normal years
    dec ecx
    jmp short .addYears
.addLeap:
    add eax, 366    ;Add the days in the leap year
    mov edx, eax    ;Save this number in edx
    ;Now to add day offset
    movzx ecx, byte [monthOfYear]
    lea rsi, monthsTbl
.addDaysInMonth:
    dec ecx ;Turn ecx to a 0 based count and decrement
    jecxz .addMonthOffset
    lodsb   ;Get the number of days in the month in al
    movzx eax, al
    add edx, eax
    jmp short .addDaysInMonth
.addMonthOffset:
    movzx ecx, byte [dayOfMonth]
    dec ecx
    add edx, ecx    ;Add month offset
    ;edx has the number of days since 01/01/1980
    mov word [daysOffset], dx   ;Store!
;Now read and then write CLOCKrecrd
    lea rbx, CLOCKrecrd ;Read into clock record
    mov ecx, 6
    xor rbp, rbp    ;Tell it we are a chardev
    call primReqReadSetup   ;rbx now points to request header
    mov rsi, qword [clockPtr]   ;Get clock driver pointer
    call goDriver
;Now we change daycount and write it back
    movzx eax, word [daysOffset]    ;Get day offset into eax
    lea rbx, CLOCKrecrd ;Read into clock record
    mov word [rbx + clkStruc.dateWord], ax
    mov ecx, 6
    xor rbp, rbp    ;Tell it we are a chardev
    call primReqWriteSetup   ;rbx now points to request header
    mov rsi, qword [clockPtr]   ;Get clock driver pointer
    call goDriver
    call setDayofWeek
    return
.exitBad:
    mov al, -1
    return

readDateTimeRecord:
;Will read the clock using the primary request header
;Preserves all regs except eax and flags
    push rbx
    push rcx
    push rdx
    push rsi
    push rbp

    mov word [keybTicks], 0 ;Reset ticks as we are gonna read time now
    lea rbx, CLOCKrecrd ;Read into clock record
    mov ecx, 6
    xor rbp, rbp    ;Tell it we are a chardev
; rbp = DPB ptr | NullPtr if a char dev
; rbx = Data storage buffer ptr
; ecx = Number of sectors to transfer
; rdx = Starting sector to read/write from/to | Undefined if a char dev
    call primReqReadSetup   ;rbx now points to request header
    mov rsi, qword [clockPtr]   ;Get clock driver pointer
    call goDriver
    movzx eax, word [CLOCKrecrd + clkStruc.dateWord]
;Here we have:  ax = Days since 1/1/1980
;Updates the internal date fields
;Internal time fields are in the clock record
;So now we check if the number of days since 1980 are the same.
;If so, we can skip updating.
    cmp ax, word [daysOffset]
    je .exit    ;Return if equal
    cmp eax, 365*120 + 25 + 5  ;Number of days until max date, 31/12/2099
    jnb .exit  ;If it is equal or above (implying we are past max date), exit
    mov word [daysOffset], ax   ;Save this new offset
    ;Now want to update years, month and day fields
    ;ax has the number of days since 01/01/1980
    ;Get days in a 4 year period
    mov ebx, 365*4 + 1
    xor edx, edx
    mov ecx, edx  ;Zero ecx
    div ebx ;Divide # of days since 01/01/1980 into 4 year bunches
    ;eax has # of 4 year bunches elapsed since 01/01/1980
    ;edx has offset into current 4 year bunch
    mov ebx, 366    ;Number of days in a leap year
    cmp edx, ebx    ;Are we in the first year of this bunch? (The leap year)
    jb .foundBunch
    sub edx, ebx    ;Subtract edx to hone in on the year we are in
    inc ecx
    dec ebx ;Number of days in a normal year
.findBunch:
    cmp edx, ebx
    jb .foundBunch
    sub edx, ebx
    inc ecx
    jmp short .findBunch
.foundBunch:
    ;If ecx = 0, february has 29 days!
    ;eax has number of four year bunches elapsed since 1980
    shl eax, 2  ;Multiply by 4 to get # of years to start of four year bunch
    add ecx, eax    ;Add # of years offset in 4 year bunch
    ;ecx now has the number of years since 1980
    call setDaysInFeb   ;Return in rax the # of days in feb
;edx has number of days into the year we are in
;eax has number of days in Feb for this year
    lea rsi, monthsTbl
    xor ecx, ecx    ;Use as month counter
.monthSearch:
    inc ecx ;Start from 1
    stosb   ;Get value in al and inc rsi
    movzx eax, al   ;Zero extend number of days in month
    cmp edx, eax
    jb .monthFound
    sub edx, eax  ;Reduce the number of days by the number of days in the month
    jmp short .monthSearch
.monthFound:
    ;cl has the month (01h-0Ch)
    ;dl has the day of the month - 1
    inc edx
    mov byte [monthOfYear], cl
    mov byte [dayOfMonth], dl
    call setDayofWeek
.exit:
    pop rbp
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    return  ;Return with ZF is date set, or same day

setDayofWeek:
;Sets in the variable the day of the week. 0 = Sunday, 6 = Saturday
    movzx eax, word [daysOffset]   ;Get the offset of days since 1/1/1980
    add eax, 2   ;1/1/1980 was on a Tuesday so shift up by two
;Take Modulo 7. 7 is non base 2 number so divide instead of anding
    xor edx, edx
    mov ecx, 7   
    div ecx  ;Get remainder in dl
    mov byte [dayOfWeek], dl
;Set ZF to indicate all ok
    xor eax, eax
    return

setDaysInFeb:   ;Int 4Fh AX = 121Bh, Set days in february
;Every fourth year has an extra day in feb (!! 2100 exception!!)
;Input: cl = Year - 1980
;Output: al = Number of days in February
    mov byte [years], cl    ;Save this figure
    test cl, 3   ;Every fourth year, the year is 0
    push rcx
    push rdi
    mov eax, 28  ;Usual days in february
    mov ecx, 29  ;Leap year days in february
    cmovz eax, ecx  ;If this is the case move 29 into eax
    lea rdi, qword [monthsTbl + 1]  ;Get second entry in table (Feb is month 2)
    stosb   ;Store the word in rdi
    pop rdi
    pop rcx
    return