bits 16
org 0x0000

%define BOCHS_BREAKPOINT xchg bx,bx

%define FLAG_IMMEDIATE 0x80
%define FLAG_HIDDEN 0x20

%define STATE_INTERPRETING 0
%define STATE_COMPILING 1

%macro NEXT 0
    nop
    nop
    lodsw
    xchg bx,ax
    mov ax, [bx]
    xchg bx,ax
    jmp bx
    int 0x3
    int 0x3
%endmacro

%macro PUSHRSP 1
    lea bp, [bp-2]
    mov [bp], %1
%endmacro

%macro POPRSP 1
    mov %1, [bp]
    lea bp, [bp+2]
%endmacro


%assign WORDLIST_LINK 0

; name, namelen, flags, label
%macro DEFWORD 4
align 2, db 0
dcw_%4:
    dw WORDLIST_LINK
    %define WORDLIST_LINK dcw_%4
    db %2
    db %3
    align 2, db 0
    db %1
    align 2, db 0
%4:
    dw DOCOL
%endmacro

; name, namelen, flags, label
%macro DEFCODE 4
align 2, db 0
dcn_%4:
    dw WORDLIST_LINK
    %define WORDLIST_LINK dcn_%4
    db %2
    db %3
    align 2, db 0
    db %1
    align 2, db 0
%4:
    dw dcn_code_%4
dcn_code_%4:
%endmacro

; name, namelen, flags, label, value
%macro DEFVAR 5
    align 2, db 0
    DEFCODE %1, %2, %3, %4
    align 2, db 0
    push dfv_%4
    NEXT
    align 2, db 0
dfv_%4:
    dw %5
%endmacro

; name, namelen, flags, label, value
%macro DEFCONST 5
    align 2, db 0
    DEFCODE %1, %2, %3, %4
    align 2, db 0
    push %5
    NEXT
%endmacro

jmp _init

align 2, db 0
frogfort:
; [PROGRAM START]
    dw LIT, 0x0010, LIT, 0x0020, PARSEWORD
    dw BREAK
    dw DONE
    dw WORDS, PNLN
    dw INTERPRET
    dw BREAK
    dw DONE
    dw LATEST, BREAK
    dw LATEST, FETCH, HIDDEN
    dw BREAK
    dW DONE


    dw CONST_F_IMMEDIATE, POPANDPRINTSTACK, PNLN
    dw CONST_F_HIDDEN, POPANDPRINTSTACK, PNLN
    dw DONE

    dw WORDS, PNLN
    dw STATE, PNLN
    dw POPANDPRINTSTACK
    dw BREAK
    dw RBRAC
    dw BREAK
    dw LBRAC
    dw BREAK
    dw WORDS
    dw PNLN


    dw LIT, 0x0008
    dw LIT, A_STR
    dw CREATE

    dw LIT, 0x0008
    dw LIT, B_STR
    dw CREATE

    dw PNLN
    dw WORDS
    dw PNLN

    dw LIT, 0x8518
    dw COMMA
    dw LIT, 0x6884
    dw COMMA



    dw DONE

    ;dw LATEST, POPANDPRINTSTACK, PNLN, LIT, 0x6884, POPANDPRINTSTACK, PNLN, DONE
    ;dw READSTRING, PNLN, LIT, input_buffer, PRINTSTRING, PNLN, DONE
    ;dw PUSH0, ADD1, ADD3, PNLN, PHEX, PNLN, PNLN, READSTRING, DONE

align 2, db 0
A_STR:
    db 'NEWWORD1'
B_STR:
    db 'NEWWORD2'


align 2, db 0
retstack_start: dw 0
datastack_start: dw 0
frogfort_start: dw 0


align 2, db 0
hexchars: db '0123456789abcdef'
align 2, db 0
msg_perror: db 'errcode:0x', 0
align 2, db 0
msg_status: db 'status:0x', 0

align 2, db 0
msg_frogfort_loaded: db 'FrogFort init complete.', 0xa,0xd,0x0



align 2, db 0
retstack_bottom:
    ; 255 bytes return stack, 128 words should be enough for everyone
    times 0xff db 0x0
    ; times 0x40 db 'CALL'
retstack_top:

align 2, db 0
datastack_bottom:
    ; 2 KiB datastack, 1024 words should be enough for everyone
    times 0x800 db 0x0
    ;times 0x200 db 'DATA'
datastack_top:

%define INPUT_BUFFER_LENGTH 0xff
input_buffer: times INPUT_BUFFER_LENGTH db 0x0
%define SCRATCH_BUFFER_LENGTH 0xff
scratch_buffer: times SCRATCH_BUFFER_LENGTH db 0x0


align 2, db 0
DOCOL:
    PUSHRSP si
    add ax, 2
    mov si, ax
    NEXT

align 2, db 0
_init:
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
 
    mov bp, retstack_top
    mov sp, datastack_top
    mov word [retstack_start], bp
    mov word [datastack_start], sp
    mov word [frogfort_start], frogfort
    BOCHS_BREAKPOINT
    mov word [dfv_HERE], START_OF_USER_DICT

    mov si, msg_frogfort_loaded
    call _printstr
    jmp _xxx
    debug_str1: db 'DONE is at ',0
    debug_str2: db 0xa,0xd,'input_buffer is at ',0
    debug_str3: db 0xa,0xd,'frogfort is at ',0
_xxx:
    mov si, debug_str1
    call _printstr
    mov ax, DONE
    call _print_ax_as_hex
    mov si, debug_str2
    call _printstr
    mov ax, input_buffer
    call _print_ax_as_hex
    mov si, debug_str3
    call _printstr
    mov ax, frogfort
    call _print_ax_as_hex
    call _printnewline

    mov si, frogfort
    NEXT




_printstr:
    push si
    mov ax,0x0e00
__printstr_1:
    lodsb
    cmp al, 0
    je __printstr_end
    int 0x10
    jmp __printstr_1
__printstr_end:
    pop si
    ret

_print_al_as_hex:
    push si
    mov si, hexchars
    mov dx, ax
    mov bx, ax
    mov cl, 4
    mov ah, 0xe
    and bx,0xf
    mov al, [bx+si]
    push ax
    mov bx, dx
    shr bx, cl
    and bx, 0xf
    mov al, [bx+si]
    int 0x10
    pop ax
    int 0x10
    pop si
    ret


_print_ax_as_hex:
    push ax
    mov cl, 8
    shr ax, cl
    and ax, 0xff
    call _print_al_as_hex
    pop ax
    and ax, 0xff
    call _print_al_as_hex
    ret

_printnewline:
    push ax
    mov ax,0x0e0a
    int 0x10
    mov al, 0x0d
    int 0x10
    pop ax
    ret

_pstatus:
    push si
    push ax
    mov si, msg_status
    call _printstr
    pop ax
    call _print_ax_as_hex
    call _printnewline
    pop si
    ret

_perror:
    push si
    push ax ; error code in ax, save, print error message prefix
    mov si, msg_perror
    call _printstr
    pop ax ; restore error code
    call _print_ax_as_hex
    call _printnewline
    pop si
    ret

_strequal:
; (ax = str1) (bx = str2) (cx = length)
; (returns 1 in ax if strings are equal, 0 if different)
    push si
    push di
    mov si, ax
    mov di, bx
    rep cmpsb
    test cx,cx
    jnz _strequal_1
    mov ax,1
    jmp _strequal_end
_strequal_1:
    xor ax,ax
_strequal_end:
    pop di
    pop si
    ret


DEFVAR 'LATEST', 6, 0, LATEST, dcn_DONE
DEFVAR 'HERE', 4, 0, HERE, 0
DEFVAR 'STATE', 5, 0, STATE, 0

DEFCONST "F_HIDDEN",8,0, CONST_F_HIDDEN, FLAG_HIDDEN
DEFCONST "F_IMMEDIATE", 11, 0, CONST_F_IMMEDIATE, FLAG_IMMEDIATE

align 2, db 0
DEFCODE 'PUSH0', 5, 0, PUSH0
    xor ax,ax
    push ax
NEXT

DEFCODE 'ADD1', 4, 0, ADD1
    pop ax
    inc ax
    push ax
NEXT

DEFCODE 'EXIT', 4, 0, EXIT
    POPRSP si
NEXT

DEFCODE 'PHEX', 4, 0, PHEX
    pop ax
    call _print_ax_as_hex    
NEXT

DEFCODE 'PNLN', 4, 0, PNLN
    mov ax,0x0e0a
    int 0x10
    mov al, 0x0d
    int 0x10
NEXT


DEFCODE '.', 1, 0, POPANDPRINTSTACK
    pop ax
    call _print_ax_as_hex
NEXT

DEFCODE 'DROP', 4, 0, DROP
    pop ax
NEXT

DEFCODE 'DUP', 3, 0, DUP
    mov bx, sp
    mov word ax, [bx]
    push ax
NEXT



; note - keeping this as a sample of DEFWORD usage,
; remove when there are others and implement
; incr1,....,incr4 in asm
DEFWORD 'ADD3', 4, 0, ADD3
    dw ADD1
    dw ADD1
    dw ADD1
    dw EXIT
NEXT

; ( addr len -- )
DEFCODE 'PRINTS', 6, 0, PRINTS
    pop cx
    pop ax
    push si
    mov ah,0xe0
    mov ax,1
_PRINTS_1:



NEXT

DEFCODE 'PRINTSZ', 7, 0, PRINTSZ
    pop ax
    push si
    mov si, ax
    call _printstr
    pop si
NEXT

DEFCODE 'LIT', 3, 0, LIT
    lodsw
    push ax
NEXT


; this is.. not great, work on it some more when needed
DEFCODE 'READSTRING', 10, 0, READSTRING
    call _READSTRING_START
    NEXT

_READSTRING_START:
    xor cx,cx
    mov di, input_buffer
_readstring_start:
    xor ax,ax
    int 0x16
    cmp al, 0xd  ; newline
    je _readstring_done
    ; echo whatever was input
    mov ah,0xe
    int 0x10

    ; was it backspace?
    cmp al, 0x8 ; backspace
    jne _readstring_inputok

    ; handle backspace
    ; jump back
    cmp cx, 0
    je _readstring_start
    dec cx
    dec di
    jmp _readstring_start

    ; wasn't backspace, store content
_readstring_inputok:
    stosb
    inc cx

    ; ensure input buffer does not overflow
    cmp cx, INPUT_BUFFER_LENGTH-1
    jge _readstring_done

    jmp _readstring_start
_readstring_done:
    pop bx ; grab return address
    push cx ; store amount of read chars
    push input_buffer ; and buffer addr for convenience
    push bx ; put return address on top
    ret


DEFCODE 'STREQU', 6, 0, STREQU
    pop cx
    pop bx
    pop ax
    call _strequal
    push ax
NEXT

; read up to len non-space characters from addr 
; store chars up to first space chars found
; consume remaining chars until non-space char 
; is found, up to len
; return address and length of parsed chars
; and address of first non-space char if found
; null if no remaining non-space chars
; ( addr len -- addr len addr_of_next_non_space_char)
DEFCODE 'PARSEWORD', 9, 0, PARSEWORD
BOCHS_BREAKPOINT
    call _PARSEWORD_START
    NEXT

%define local1 bp-2
%define local2 bp-4
%define local3 bp-6
%define local4 bp-8
%define local5 bp-10
%define local6 bp-12
%define arg1 bp+4
%define arg2 bp+6
%define arg3 bp+8

_PARSEWORD_START:
; todo - stuff here is currently for reference
; about stack state when entering parseword
; start scanning through string pointed at by addr
; copy chars to scratchbuffer until ' ' is found
; store number of chars read at that point into a
; local. keep scanning until a non-space char is found
; store addr of first non-space char in local
; clean up, setting return stack to
; - return address
; - addr_of_non_space_char
; - number of chars read into scratch_buffer
; - scratch_buffer

    push bp
    push si
    mov bp, sp
    sub sp, 12
    mov word [local6], 



    

; exit
    mov sp, bp
    pop si
    mov bx,bp
    pop bp

    pop ax ; return address
    add sp, 4 ; function took two word sized params
    push word [bx+2]
    push ax ; return address
    ret

DEFCODE 'WORDS', 5, 0, WORDS
    push si
    mov bx, [dfv_LATEST]
_WORDS_START:
    mov si, bx 
    add si, 4; si should now point at word name
    test bx, bx
    je _WORDS_END

    push bx
    mov ah,0x0e
    mov al, '['
    int 0x10
    mov ax, bx
    call _print_ax_as_hex
    pop bx
    mov ah,0x0e
    mov al,':'
    int 0x10

    xor ax,ax
    mov al, byte [bx+2] ; name length offset
    ;and al, 0x1f
    xor cx,cx
    mov cl, al
    mov ah, 0x0e
    push bx
    xor bx,bx
_WORDS_PRINTWORDNAME:
    lodsb
    int 0x10
    loop _WORDS_PRINTWORDNAME
    mov ah,0x0e
    mov al,']'
    int 0x10
    mov al,' '
    int 0x10
    pop bx
    mov bx, word [bx]
    jmp _WORDS_START
_WORDS_END:
    pop si
NEXT

; ( addr len -- maybe_addr?)
; scan through dictionary looking or word matching name found at addr
DEFCODE 'FIND', 4, 0, FIND
    pop di
    pop cx
    call _FIND_IMPL
    ; store found word addr, 0 for not found
    push ax
NEXT

_FIND_IMPL:
    ; we'll be trashing si, so save it
    push si
    ; start scanning at the latest word defined
    mov bx, [dfv_LATEST]

_FIND_IMPL_CHECK_WORD:
    test bx,bx
    je _FIND_IMPL_EXIT

    mov al, byte [bx+2] ; name length offset
    cmp al, cl
    ; word name lengths are different, proceed to next
    jne _FIND_IMPL_NEXT_WORD

    ; word name lengths are equal, compare names
    mov si, bx
    add si, 4 ; si now points at start of word name
    ; save bx and cx, since _strequal will trash them
    push bx
    push cx
    ; name ptr1 in ax, name ptr2 in bx, length in cx
    mov ax, si
    mov bx, di
    call _strequal

    pop cx
    pop bx

    ; strings are equal if ax = 1
    cmp ax,1
    jne _FIND_IMPL_NEXT_WORD
    ; ax = 1, word matched, move word address into ax
    ; and exit
    jmp _FIND_IMPL_EXIT

_FIND_IMPL_NEXT_WORD:
    ; move pointer to next (well, previous, really)
    ; word in dictionary
    mov bx, word [bx]
    jmp _FIND_IMPL_CHECK_WORD

_FIND_IMPL_EXIT:
    ; return address of current word in ax
    ; it will be either 0 if the whole dictionary was
    ; traversed, or the address of the matching word
    mov ax, bx
    ; restore si since it was used in comparison
    pop si
    ret
    int 0x3
    ; TODO IMPLEMENT


DEFCODE 'EMIT', 4, 0, EMIT
    pop ax
    mov ah,0x0e
    push bx
    int 0x10
    pop bx
NEXT

; ( addr len -- )
DEFCODE 'CREATE', 6, 0, CREATE
BOCHS_BREAKPOINT
    pop bx
    pop cx
    mov di, word [dfv_HERE]
    mov ax, word [dfv_LATEST]
    stosw ; store pointer to previously defined word
    mov ax, cx
    stosb ; store length
    xor ax,ax
    stosb ; store flags (0 by default)

    ; copy name
    push si
    mov si, bx 
    ; cx still has length of name
    rep movsb
    pop si

    ; update latest-word-defined pointer
    mov ax, [dfv_HERE]
    mov [dfv_LATEST], ax
    ; TODO - align di to word boundary
    mov [dfv_HERE], di

NEXT

DEFCODE ',', 1,0, COMMA
    pop ax
    call _COMMA
NEXT
_COMMA:
    mov di, [dfv_HERE]
    stosw
    mov [dfv_HERE], di
    ret

DEFCODE 'BREAK', 1, 0, BREAK
    BOCHS_BREAKPOINT
NEXT

DEFCODE '[', 1, FLAG_IMMEDIATE, LBRAC
    mov word [dfv_STATE], STATE_COMPILING
NEXT

DEFCODE ']', 1, 0, RBRAC
    mov word [dfv_STATE], STATE_INTERPRETING
NEXT

DEFWORD ':', 1, 0, COLON
dw 1234
dw CREATE
dw LIT, DOCOL, COMMA
dw LATEST, FETCH

; 0x3b = ; - written as this to avoid editor
; considering the rest of the line a comment
DEFWORD 0x3b, 1, FLAG_IMMEDIATE, SEMICOLON
dw LIT, EXIT, COMMA
dw LATEST, FETCH, HIDDEN
dw LBRAC
dw EXIT

DEFCODE '@',1,0,FETCH
    pop bx
    mov ax, [bx]
    push ax
NEXT


DEFCODE 'IMMEDIATE', 9, FLAG_IMMEDIATE, IMMEDIATE
    mov di, [dfv_LATEST]
    add di, 3 ; di now points at flags byte
    xor byte [di], FLAG_IMMEDIATE
NEXT

DEFCODE 'HIDDEN', 6, 0, HIDDEN
BOCHS_BREAKPOINT
    pop di
    add di, 3
    xor byte [di], FLAG_HIDDEN
NEXT


DEFCODE 'INTERPRET', 9, 0, INTERPRET
    call _READSTRING_START
    BOCHS_BREAKPOINT
NEXT

DEFCODE 'DONE', 4, 0, DONE
BOCHS_BREAKPOINT
    jmp _done_loop
_done_text: db 'FrogFort stopped.',0xa,0xd,0x0
_done_loop:
    push si
    mov si, _done_text
    call _printstr
    pop si
    xor ax, ax
    int 0x16
    jmp _done_loop
NEXT ; note that this will not get called

align 2, db 0
START_OF_USER_DICT:
    db 'frog'


times 65535-($-$$) db 0xcc
