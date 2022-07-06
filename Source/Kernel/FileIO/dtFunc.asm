;Major kernel date time functions and subroutines

getDate:           ;ah = 2Ah
setDate:           ;ah = 2Bh
getTime:           ;ah = 2Ch
setTime:           ;ah = 2Dh
    return

;------------------------
;   Utility functions   :
;------------------------
readDateTimeRecord:
;Will read the clock using the primary request header
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
    movzx ecx, word [CLOCKrecrd + clkStruc.minutes]    ;cl = Mins, ch = Hours
    movzx edx, word [CLOCKrecrd + clkStruc.hseconds]   ;dl = hSecs, dh = Secs
;Here we have:  ax = Days since 1/1/1980, 
;               cl = Minutes, ch = Hours, 
;               dl = HSeconds, dh = Seconds
;
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