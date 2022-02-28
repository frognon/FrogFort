bits 16
org 0x0000

%define WORD_SIZE 2

%define BOCHS_BREAKPOINT xchg bx,bx

%define FLAG_IMMEDIATE 0x80
%define FLAG_HIDDEN 0x20

%define STATE_INTERPRETING 0
%define STATE_COMPILING 1

%define LOCAL1 bp-2
%define LOCAL2 bp-4
%define LOCAL3 bp-6
%define LOCAL4 bp-8
%define LOCAL5 bp-10


%macro print_msg_and_word_in_hex 2
    push word %1
    call _printsz
    mov ax, %2
    call _print_ax_as_hex
%endmacro


%macro NEXT 0
    ;BOCHS_BREAKPOINT
    nop
    nop
    lodsw
    mov bx, ax ; need to do this, since unable to
    jmp [bx]   ; do jmp [ax] in realmode

    int 0x3 ; clean out these at some point
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

; name, flags, label
%macro DEFWORD 3
align 2, db 0
dcw_%3:
    dw WORDLIST_LINK
    %define WORDLIST_LINK dcw_%3
    %strlen %%namelen %1
    db %%namelen
    db %2
    align 2, db 0
    db %1
    align 2, db 0
dcw_definition_backpointer_%3:
    dw dcw_%3
%3:
    dw DOCOL
%endmacro

; name, flags, label
%macro DEFCODE 3
align 2, db 0
dcn_%3:
    dw WORDLIST_LINK
    %define WORDLIST_LINK dcn_%3
    %strlen %%namelen %1
    db %%namelen
    db %2
    db %1
    align 2, db 0
dcn_definition_backpointer_%3:
    dw dcn_%3
%3:
    dw dcn_code_%3
dcn_code_%3:
%endmacro

; name, flags, label, value
%macro DEFVAR 4
    align 2, db 0
    DEFCODE %1, %2, %3
    align 2, db 0
    push dfv_%3
    NEXT
    align 2, db 0
dfv_%3:
    dw %4
%endmacro

; name, flags, label, value
%macro DEFCONST 4
    align 2, db 0
    DEFCODE %1, %2, %3
    align 2, db 0
    push %4
    NEXT
%endmacro

jmp _init

align 2, db 0
frogfort:
; [PROGRAM START]

    dw INTERPRET
    dw PUSH0
    dw ADD1

    dw ZEROBRANCH 
    dw 4
    dw LIT, 0x1234, PHEX, DONE
    dw LIT, 0x6969, PHEX, DONE

    dw DONE

    dw DONE
    dw INTERPRET
    dw DONE

    dw LIT
    dw 0x1234
    dw PHEX
    dw DONE
    dw DONE

    ;dw LATEST, POPANDPRINTSTACK, PNLN, LIT, 0x6884, POPANDPRINTSTACK, PNLN, DONE
    ;dw READSTRING, PNLN, LIT, input_buffer, PRINTSTRING, PNLN, DONE
    ;dw PUSH0, ADD1, ADD3, PNLN, PHEX, PNLN, PNLN, READSTRING, DONE

align 2, db 0
A_STR:
    db 'NEWWORD1'
align 2, db 0
B_STR:
    db 'NEWWORD2'

align 2, db 0
MEMCHR_TEST_STRING1:
    db 'hello world!',0
    times 255 db 0xff

align 2, db 0
MEMNOTCHR_TEST_STRING1:
    db '  ',0x9,0xa,0xd, '    DONE token2  token3                token4'
    times(255-($-MEMNOTCHR_TEST_STRING1)) db 0

align 2, db 0
MEMNOTCHR_TEST_STRING3:
    db '  ',0x9,0xa,0xd, ;'token1   token2  token3                token4'
    times(255-($-MEMNOTCHR_TEST_STRING3)) db 0

align 2, db 0
MEMCMP_S1: db 'hello'
MEMCMP_S2: db 'hexlo'
;times 65535-($-$$) db 0xcc


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
    ;times 0xff db 0x0
    times 0x40 db 'CALL'
retstack_top:

align 2, db 0
datastack_bottom:
    ; 2 KiB datastack, 1024 words should be enough for everyone
    ;times 0x800 db 0x0
    times 0x200 db 'DATA'
datastack_top:

%define INPUT_BUFFER_LENGTH 0xff
align 2, db 0
input_buffer: times INPUT_BUFFER_LENGTH db 0x0
%define SCRATCH_BUFFER_LENGTH 0xff
align 2, db 0
scratch_buffer: times SCRATCH_BUFFER_LENGTH db 0x0

; this space is used for compiling new word definitions into
; finished, and valid word definitions, are appended to the dictionary
; as expected. using a temporary buffer allows "rollback" in case the
; word being defined ends up being unusable (e.g. refering to missing words)
%define TEMPORARY_WORD_COMPILATION_BUFFER_MAX_LENGTH 0x200 

align 2, db 0
TWCB_len:
temporary_word_compilation_buffer_content_length: dw 0

align 2, db 0
TWCB:
temporary_word_compilation_buffer: times TEMPORARY_WORD_COMPILATION_BUFFER_MAX_LENGTH db 0x0

align 2, db 0
TWCB_ptr:
temporary_word_compilation_buffer_ptr: dw temporary_word_compilation_buffer

; appends word in register named in param1 into TWCB
%macro push_twcb 1
    push bx
    mov bx, [TWCB_ptr]
    mov [bx], %1
    add word [TWCB_ptr], 2
    add word [TWCB_len], 2
    pop bx
%endmacro


align 2, db 0
DOCOL:
    ;BOCHS_BREAKPOINT
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
    ;BOCHS_BREAKPOINT
    mov word [dfv_HERE], START_OF_USER_DICT

    push word msg_frogfort_loaded
    call _printsz

    jmp _output_interesting_memory_offsets

    debug_str1: db 'DONE is at ',0
    debug_str2: db 0xa,0xd,'input_buffer is at ',0
    debug_str3: db 0xa,0xd,'frogfort is at ',0
    debug_str4: db 0xa,0xd,'retstack top at ', 0
    debug_str5: db 0xa,0xd,'datastack top at ', 0
    debug_str6: db 0xa,0xd,'HERE is at ', 0
    debug_str7: db 0xa,0xd,'INTERPRET is at ', 0
    debug_str8: db 0xa,0xd,'TWCB is at ', 0
    debug_str9: db 0xa,0xd,'ADD1 is at ', 0

_output_interesting_memory_offsets:
    print_msg_and_word_in_hex debug_str1, DONE
    print_msg_and_word_in_hex debug_str2, input_buffer
    print_msg_and_word_in_hex debug_str3, frogfort
    print_msg_and_word_in_hex debug_str4, retstack_start
    print_msg_and_word_in_hex debug_str5, datastack_start
    print_msg_and_word_in_hex debug_str6, dcn_HERE
    print_msg_and_word_in_hex debug_str7, dcn_INTERPRET
    print_msg_and_word_in_hex debug_str8, TWCB
    print_msg_and_word_in_hex debug_str9, ADD1

    call _printnewline


    jmp _testcode_start
align 2, db 0
_atoi_test_string: db '1234'

_testcode_start:
    ;BOCHS_BREAKPOINT
    push _atoi_test_string
    push 4
    call _atoi
    pop cx
    pop ax

    ;BOCHS_BREAKPOINT



_begin_forth_execution:
    ; start built in program execution
    ;BOCHS_BREAKPOINT
    mov si, frogfort
    NEXT




; BEGIN BUILTIN NATIVE ROUTINES
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
    push word msg_status
    call _printsz
    pop ax
    call _print_ax_as_hex
    call _printnewline
    pop si
    ret

_perror:
    push si
    push ax ; error code in ax, save, print error message prefix
    mov si, msg_perror
    call _printsz
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

; FIXME - why is this like this, why are we messing with bx
_printsz:
    mov bx, sp
    push bp
    push si
    mov bp, bx
    mov si, [bp+2]
    xor bx,bx
    mov ax,0x0e00
.printsz_1:
    lodsb
    cmp al, 0
    je .printsz_end
    int 0x10
    jmp .printsz_1
.printsz_end:
    pop si
    pop bp
    pop bx
    add sp, 2
    push bx
    ret

_readword:
    ; todo use stack instead of cx for length counter
    xor cx,cx
    mov di, input_buffer
.readchar:
    call _key
    mov ah,0xe ; echo whatever char was input
    int 0x10
    pop ax
    cmp al, 0x20 ; space
    je .done
    cmp al, 0x8 ; backspace
    jne .notbackspace
    dec di
    dec cx
    jmp .readchar

.notbackspace:
    stosb
    inc cx
    jmp .readchar

.done:
    pop dx ; return address

    push cx ; number of chars read
    push input_buffer ; address of input buffer
    push dx ; return address

    ret
    

    ;*s c n
_memset:
    push bp
    mov bp, sp
    add sp, 4
    pop cx
    pop ax
    pop di
    ; todo - optimize this to use stosw
    ; by dividing cx by two, checking lsb of cx
    ; to determine if bytecount is even or odd
    ; if even, just do count / 2 STOSWs (after setting ah <- al)
    ; if odd, blank lsb, do count / 2 STOSWs and one stosb (after setting ah <0 al)
    rep stosb
    mov sp, bp
    pop bp
    pop ax
    add sp, 6
    push ax
    ret

_readstring:
    ; clear input buffer

    ; addr
    push input_buffer

    ; fill with zeroes
    xor ax,ax
    push ax

    ; length
    mov cx, INPUT_BUFFER_LENGTH
    push cx

    call _memset

    xor cx,cx
    mov di, input_buffer

_readstring_start:
    push cx ; store number of chars read so far, functions need not preserve AX,BX,CX,DX
    call _key
    pop ax ; return value from function
    pop cx ; number of chars read so far


     ; was it a newline = end of input?
    cmp al, 0xd  ; newline
    jne .notnewline

    ; output 0xa 0xd to move to next line
    mov ax,0x0e0a
    int 0x10
    mov al, 0xd
    int 0x10
    ; store newline in buffer?
    mov al, 0xa
    stosb
    mov al, 0
    ; store terminating zero
    stosb

    ; we've added a newline and a terminating 
    ; zero to the buffer, increase charcount by two
    add cx, 2
    jmp _readstring_done

.notnewline:
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
    ; todo - handle more keys here if desired
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


; END BUILTIN NATIVE ROUTINES


; BEGIN BUILTIN FORTH DEFINITIONS
DEFVAR 'LATEST', 0, LATEST, dcn_DONE
DEFVAR 'HERE', 0, HERE, 0
DEFVAR 'STATE', 0, STATE, 0

DEFCONST "F_HIDDEN", 0, CONST_F_HIDDEN, FLAG_HIDDEN
DEFCONST "F_IMMEDIATE", 0, CONST_F_IMMEDIATE, FLAG_IMMEDIATE

align 2, db 0
DEFCODE 'PUSH0', 0, PUSH0
    xor ax,ax
    push ax
NEXT

DEFCODE 'ADD1', 0, ADD1
    pop ax
    inc ax
    push ax
NEXT

DEFCODE 'EXIT', 0, EXIT
    POPRSP si
NEXT

DEFCODE 'PHEX', 0, PHEX
    pop ax
    call _print_ax_as_hex    
NEXT

DEFCODE 'PNLN', 0, PNLN
    mov ax,0x0e0a
    int 0x10
    mov al, 0x0d
    int 0x10
NEXT


DEFCODE '.', 0, POPANDPRINTSTACK
    pop ax
    call _print_ax_as_hex
NEXT

DEFCODE 'DROP', 0, DROP
    pop ax
NEXT

DEFCODE 'DUP', 0, DUP
    mov bx, sp
    mov word ax, [bx]
    push ax
NEXT

DEFCODE 'ROT', 0, ROT
    mov bx, sp
    mov ax, [bx]
    mov dx, [bx+2]
    mov [bx+2], ax
    mov [bx], dx
NEXT

; ( n m -- n)
DEFCODE '+', 0, OP_ADD
    pop ax
    pop dx
    add ax, dx
    push ax
NEXT

DEFCODE '>R', 0, TO_RSTACK
    pop ax
    PUSHRSP ax
NEXT

DEFCODE 'R>', 0, FROM_RSTACK
    POPRSP ax
    push ax
NEXT

DEFCODE '@RSP', 0, FETCH_RSP
    push bp
NEXT

DEFCODE 'RSP!', 0, STORE_RSP
    pop bp
NEXT

DEFCODE 'RDROP', 0, DROP_RSP
    add bp, 2
NEXT

DEFCODE 'DSP@', 0, FETCH_DSP
    mov ax, sp
    push ax
NEXT

DEFCODE 'DSP!', 0, STORE_DSP
    pop sp
NEXT

DEFCODE '>CFA', 0, TCFA
    BOCHS_BREAKPOINT
    call _TCFA
NEXT

_TCFA:
    ;BOCHS_BREAKPOINT
    pop dx ; return address
    pop bx ; word definition address
    add bx, 2 ; skip link wordlist link
    xor cx,cx
    mov byte cl, [bx] ; grab name length
    add bx, 2 ; advance past name length and flags
    add bx, cx ; add length of name to bx
    inc bx        ; word after name in word definition
    and bl, 0xfe  ; is 2-aligned, make sure bx is too
    add bx, 2     ; skip backpointer
    push bx       ; bx now points at code start
    push dx
    ret

DEFWORD '>DFA', 0, TDFA
    dw TCFA
    dw ADD2
    dw EXIT
NEXT

; note - keeping this as a sample of DEFWORD usage,
; remove when there are others and implement
; incr1,....,incr4 in asm
DEFWORD 'ADD3', 0, ADD3
    dw ADD1
    dw ADD1
    dw ADD1
    dw EXIT
NEXT

DEFWORD 'ADD2', 0, ADD2
    dw ADD1
    dw ADD1
    dw EXIT
NEXT

DEFCODE 'PRINTSZ', 0, PRINTSZ
    call _printsz
NEXT

DEFCODE 'LIT', 0, LIT
    lodsw
    push ax
NEXT

DEFCODE 'KEY', 0, KEY 
    call _key
NEXT
_key:
    xor ax, ax
    int 0x16
    pop dx ; return address
    push ax ; key
    push dx ; return address to top of stack
    ret


; this is.. not great, work on it some more when needed
; ( -- addr len )
DEFCODE 'READSTRING', 0, READSTRING
    call _readstring
NEXT

DEFCODE 'READWORD', 0, READWORD
    call _readword
NEXT


DEFCODE 'STREQU', 0, STREQU
    pop cx
    pop bx
    pop ax
    call _strequal
    push ax
NEXT

DEFCODE 'WORDS', 0, WORDS
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
DEFCODE 'FIND', 0, FIND
    call _find
NEXT

%define NLOCALS 5 * WORD_SIZE
%define NARGS 2 * WORD_SIZE

%define ARG_addr bp+4
%define ARG_len bp+6


_find:
    push bp
    mov bp, sp
    %if NLOCALS > 0 
    sub sp, NLOCALS ; allocate space for locals
    %endif
    mov bx, [dfv_LATEST]

.read_word_header:
    xor cx,cx
    xor ax,ax
    mov cl, [bx+2]
    mov al, [ARG_len]
    cmp ax,cx
    je .compare

.next:
    mov bx, [bx]
    test bx, bx
    jnz .read_word_header
    ; reached first word in list (so next is null)
    ; without finding key, return 0 (not found)
    xor ax, ax
    jmp .exit

.compare:
    push bx
    push ax ; length
    mov dx, [ARG_addr]
    push dx
    lea dx, [bx+4] ; word name in header
    push dx
    call _memcmp
    pop ax ; return value
    pop bx ; pointer to word header
    test ax, ax
    jnz .next ; not found
    mov ax, bx


.exit:
    %if NLOCALS > 0
    add sp, NLOCALS ; discard locals
    %endif
    pop bp
    pop bx ; return address
    add sp, NARGS ; remove parametrs from stack
    push ax ; FIND result
    push bx ; return address
    ret


DEFCODE 'EMIT', 0, EMIT
    pop ax
    mov ah,0x0e
    push bx
    int 0x10
    pop bx
NEXT

; ( addr len -- )
DEFCODE 'CREATE', 0, CREATE
    pop bx
    pop cx
    mov di, word [dfv_HERE]
    mov ax, word [dfv_LATEST]
    stosw ; store pointer to previously defined word
    mov ax, cx
    stosb ; store length
    xor ax,ax
    stosb ; store flags (0 by default)

    ; store si
    push si

    ; copy name
    mov si, bx 
    ; cx still has length of name
    rep movsb

    ; restore si after using it to store the name
    pop si

    ; update latest-word-defined pointer
    mov ax, [dfv_HERE]
    mov [dfv_LATEST], ax
    ; TODO - align di to word boundary
    mov [dfv_HERE], di

NEXT

DEFCODE ',', 0, COMMA
    pop ax
    call _COMMA
NEXT
_COMMA:
    mov di, [dfv_HERE]
    stosw
    mov [dfv_HERE], di
    ret

DEFCODE 'BREAK', 0, BREAK
    BOCHS_BREAKPOINT
NEXT

DEFCODE '[', FLAG_IMMEDIATE, LBRAC
    mov word [dfv_STATE], STATE_COMPILING
NEXT

DEFCODE ']', 0, RBRAC
    mov word [dfv_STATE], STATE_INTERPRETING
NEXT

DEFWORD ':', 0, COLON
dw 1234
dw CREATE
dw LIT, DOCOL, COMMA
dw LATEST, FETCH

DEFWORD ';', FLAG_IMMEDIATE, SEMICOLON
dw LIT, EXIT, COMMA
dw LATEST, FETCH, HIDDEN
dw LBRAC
dw EXIT

DEFCODE '@',0,FETCH
    pop bx
    mov ax, [bx]
    push ax
NEXT


DEFCODE 'IMMEDIATE', FLAG_IMMEDIATE, IMMEDIATE
    mov di, [dfv_LATEST]
    add di, 3 ; di now points at flags byte
    xor byte [di], FLAG_IMMEDIATE
NEXT

DEFCODE 'HIDDEN', 0, HIDDEN
    pop di
    add di, 3
    xor byte [di], FLAG_HIDDEN
NEXT


DEFCODE 'INTERPRET', 0, INTERPRET

    push bp
    mov bp, sp

    sub sp, 10 ; space for local wars

    %define interpret_inputstr_len bp-2
    %define interpret_inputstr_start bp-4
    %define interpret_token_addr bp-6
    %define interpret_token_len bp-8

    call _readstring

    ; retrieve pointer to inputstr start
    pop bx
    mov word [interpret_inputstr_start], bx
    ; retrieve length of inputstr
    pop cx
    mov word [interpret_inputstr_len], cx

    ; put inputstr length and start location
    ; on top of stack for initial call to _token
    push cx
    push bx



    ;BOCHS_BREAKPOINT

_interpret_tokenize:
; you keep on digging up the crossroads
; are we done? if ds:bx points at a newline, we are
; also, if ds:bx points at input_string_start + input_string_length
; we are done
    cmp byte [bx], 0xa
    jz .exterminate_exterminate_exterminate

    call _token
    pop bx
    pop cx
    test bx,bx
    jz .badtoken
    ; token found, hold on to copies of addr&len
    mov [interpret_token_addr], bx
    mov [interpret_token_len], cx

    ; pass found token addr & len to _find
    push cx ; length
    push bx ; start addr
    call _find
    pop bx ; address of possibly found word
    test bx,bx
    jnz .word_found

    ; maybe it's a litteral
    push word [interpret_token_addr]
    push word [interpret_token_len]
    call _atoi
    pop cx
    pop ax
    test cx,cx
    jnz .word_not_found

    ; so at this point we've succesfully parsed a number
    ; which now sits in ax
    ; what we need to do is push LIT, push ax into the twcb
    ; todo make a macro for appending to twcb
    ; call it something like append_to_twcb_struct
    ; have it take params for base of twcb

    mov dx, LIT
    push_twcb dx
    push_twcb ax
    jmp .next_token


;%macro print_msg_and_word_in_hex 2



.word_found:
    ; word found, address in bx

    ; fetch code field address for word
    push bx
    call _TCFA
    pop dx
    ; code field address in dx
    push_twcb dx


; fixme stupid name
.next_token:
    ; advance input parsing 
    mov bx, [interpret_token_addr]
    add bx, [interpret_token_len]

    ; TODO
    ; check if we've hit the end of the buffer
    push bx ; _token will continue from this address after jump

    jmp _interpret_tokenize


.badtoken:
    push msg_badtoken
    call _printsz
    ; TODO - reset everything?
    BOCHS_BREAKPOINT
    jmp _done

.word_not_found:
    push msg_word_not_found
    call _printsz
    ; stack still contains parsed token addr & len
    BOCHS_BREAKPOINT
    call _printstr
    ; TODO - reset everything?
    BOCHS_BREAKPOINT
    jmp _done
.exterminate_exterminate_exterminate:
    ; old-parse-pointer will be on top of stack
    ; when landing here, get rid of it
    ; fixme - cleanup the earlier stuff to avoid this in general
    pop bx 
    BOCHS_BREAKPOINT
    jmp _b
    something_new:
    db 'execing twcb',0xa,0xd,0
    _b:
    push something_new
    call _printsz

    PUSHRSP si
    mov si, TWCB
    NEXT


    BOCHS_BREAKPOINT
    jmp _done

_interpret_exit:
    ; restore stack and bp
    mov sp, bp
    pop bp
    ; done
NEXT

; TODO move this
align 2, db 0
msg_badtoken: db 'bad token ',0
align 2, db 0
msg_word_not_found: db 'word not found: ', 0


; ( addr len -- )
DEFCODE 'PRINTSTR', 0, PRINTSTR
    call _printstr

NEXT

_printstr:
    push bp
    mov bp, sp
    push word [bp+4] ; LOCAL1
    push si
    mov si, [LOCAL1]
    mov cx, [bp+6]
    xor bx,bx
    mov ah, 0x0e

.printloop:
    lodsb
    int 0x10
    dec cx
    test cx,cx
    jnz .printloop
    pop si
    mov sp ,bp
    pop bp
    pop bx ; return address
    add sp, 4 ; clear parameters
    push bx
    ret

; (addr maxlen -- number_of_unconverted_characters maybe_parsed_number)
DEFCODE 'ATOI', 0, ATOI
    call _atoi
NEXT

; very much based on the excellent implementation in jonesforth
_atoi:
    push bp
    mov bp, sp
    ; [sp] bp [sp+2] ret, [sp+4] addr, [sp+6] len

    mov cx, [bp+4] ; length
    mov di, [bp+6] ; address


    ; todo - copy buffer to temporary buffer, then uppercase it
    ; upper and lowercase strings should be equivalent for input
    ; such that abce ABCd are equal, although the algorithm
    ; requires the input string to be uppercase


; TODO - move BASE to a settable forth variable
%define BASE 10
    mov dx, BASE

    xor ax,ax
    xor bx,bx

    ; todo implement check first char is - and negate at end if it is
    ; check if first char is -, if it is, push 0 on stack
    ; dec cx since we ate a char etc, check if that was all



.atoi_loop:
    imul ax, dx
    mov byte bl, [di]
    inc di

    sub bl, '0' ; if subtracting the ascii codepoint for 0 from bl
    jb .done    ; goes below 0, input char is ascii below 0 and we're done

    cmp bl, 10  ; <= 9? 
    jb .asdf    ; if so, asdf.

    sub bl, 17  ; this is interesting, 'A'-'0' is 17, so if this goes below
    jb .done    ; 0, input char is ascii below 0 and we're done
    add bl, 10  ; however, if it is not, adding 10 offsets it into
                ; the above 10 numeric range
.asdf:
    cmp bl, dl
    jge .done

    add ax, bx
    dec cx
    jnz .atoi_loop
.done:
    ; todo check if 0 is on stack, if it is, negate ax
    mov sp, bp
    pop bp
    pop bx ; grab return address
    add sp, 4 ; remove parameters from stack
    push ax ; possibly the integer value of the string passed in
    push cx ; count of chars processed
    push bx ; return address

    ;BOCHS_BREAKPOINT
    ret

; (addr1 addr2 len -- n)
; n is zero if first len bytes of addr1
; are equal to first len bytes of addr2
DEFCODE 'MEMCMP', 0, MEMCMP
    call _memcmp
NEXT

; fixme - no need to preserve di
; fixme - consistency - use bp or bx, switching to bp adds a bit of complxty.
_memcmp:
    mov bx, sp
    push si
    push di
    mov si, [bx+2]
    mov di, [bx+4]
    mov cx, [bx+6]
    inc cx
    rep cmpsb
    mov ax, cx

.exit:
    pop di
    pop si
    pop bx ; return addr
    add sp, 6 ; clear 3 params from stack
    push ax ; comparison result
    push bx ; return addr

    ret




;(addr maxlen -- len_or_null)
_szstrnlen:
    BOCHS_BREAKPOINT
    push bp
    mov bp, sp

    push di
    mov di, [bp+6]
    mov cx, [bp+4]
    xor ax,ax ; we're looking for a null byte
    repne scasb
    test cx,cx
    jz .notfound
    mov ax, di
    sub ax, [bp+6] ; subtract starting address from found address
    jmp .exit

.notfound:
    xor ax,ax
    jmp .exit

.exit:
    pop di
    mov sp, bp
    pop bp
    pop bx ; return address
    add sp, 4 ; clear params
    push ax ; string length
    push bx ; return address
    ret








; ( addr needle maxlen -- addr_or_null )
DEFCODE 'MEMCHR', 0, MEMCHR
    call _memchr 
NEXT

_memchr:
    push bp
    push di
    mov bp, sp
    mov ax, [bp+8] ; needle parameter
    ; clear upper 8 bits, only comparing lower 8
    and ax, 0xff
    mov di, [bp+10] ; addr
    mov cx, [bp+6] ; maxlen

    repne scasb
    cmp cx, 0
    je .overflow

    ; if needle was found, di will point at that addr+1
    ; because of the `repne` prefix, fix that
    dec di
    mov ax,di
    jmp .exit

.overflow:
    xor ax,ax
    ; fallthrough

.exit:
    ; restore overwritten important registers
    mov sp, bp
    pop di
    pop bp

    ; fetch&store return address & remove parameters from stack
    pop bx
    add sp, 6
    ; store return value
    push ax
    ; store return address
    push bx
    ret


DEFCODE 'MEMNOTCHR', 0, MEMNOTCHR
    call _memnotchr
NEXT

_memnotchr:
    push bp
    push di
    mov bp, sp
    mov ax, [bp+8]
    and ax, 0xff
    mov di, [bp+10]
    mov cx, [bp+6]
    repe scasb
    cmp cx, 0
    je .overflow
    dec di
    mov ax, di
    jmp .exit

.overflow:
    xor ax,ax
    ; fallthrough

.exit:
    ; restore overwritten important registers
    mov sp, bp
    pop di
    pop bp
    ; fetch&store return address & remove parameters from stack
    pop bx
    add sp, 6
    push ax
    push bx
    ret

; returns 1 if character is ascii whitespace
_iswhitespace:
    push bp
    mov bp,sp
    mov ax, [bp+4]

    cmp al, 0x20 ; space
    je .exit1
    cmp al, 0x9 ; tab
    je .exit1
    cmp al,  0xa ; \n newline
    je .exit1

.exit0:
    xor ax,ax
    jmp .return

.exit1:
    xor ax,ax
    inc ax

.return:
    mov sp,bp
    pop bp
    pop bx ; return address
    add sp, 2 ; remove parameter from stack
    push ax ; return value
    push bx ; return address
    ret
    

; scan memory starting at addr until whitespace
; ascii character is found or maxlen bytes scanned
; ( addr maxlen -- addr_or_null )
_memws:
    push bp
    mov bp, sp

    ; local 3 [bp-6]
    ; local 2 [bp-4] ; address ptr
    ; local 1 [bp-2] ; counter
    ; old bp [bp]
    ; param0 (return address) [bp+2]
    ; param1 [bp+4] addr
    ; param2 [bp+6] maxlen
    ; ...

    sub sp, 4 ; storage for counter and address ptr

    ; initialize counter from param2 (maxlen)
    mov ax, [bp+4]
    mov [bp-2], ax

    ; initialize ptr from param1
    mov ax, [bp+6]
    mov [bp-4], ax

    xor ax,ax
.loop1:
    mov bx, [bp-4]
    mov byte al, [bx]
    push ax
    call _iswhitespace
    pop ax
    test ax,ax
    jnz .whitespacefound
    dec word [bp-2]
    jz .whitespacenotfound
    inc word [bp-4]
    jmp .loop1

.whitespacefound:
    mov word ax, [bp-4]
    jmp .exit
.whitespacenotfound:
    xor ax,ax
.exit:
    mov sp, bp
    pop bp
    pop bx ; return address
    add sp, 4
    push ax ; return value
    push bx ; return address
    ret
    


    
; scan memory starting at addr until non-whitespace
; ascii character is found or more than maxlen bytes scanned
; ( addr maxlen -- addr_or_null )
DEFCODE 'MEMNOTWS', 0, MEMNOTWS
    call _memnotws
NEXT
_memnotws:
    push bp
    push si
    mov bp, sp
    mov cx, word [bp+6]
    mov si, word [bp+8]


.loop1:
    lodsb
    cmp al, ' '
    jz .loop1
    cmp al, 0xa ; \n
    jz .loop1
    cmp al, 0xd ; \r
    jz .loop1
    cmp al, 0x9 ; '\t
    jz .loop1

    dec cx
    jz .exit_notfound


.exit_found:
    ; si already points at next byte, use previous (scanned) byte
    dec si
    mov ax, si
    jmp .exit

.exit_notfound:
    xor ax,ax
    ; fallthrough

.exit:
    ; ax will point at first non-whitespace byte, or null if
    ; more than maxlen bytes scanned
    mov sp, bp
    pop si
    pop bp
    pop bx ; return address from stack
    add sp, 4
    push ax ; put pointer to non-ws char on stack
    push bx ; put return address back on stack
    ret

; ( dest src len -- )
DEFCODE 'MEMCPY', 0, MEMCPY
    BOCHS_BREAKPOINT
    call _memcpy
NEXT
_memcpy:
    push bp
    push si
    push di
    mov bp, sp
    mov cx, word [bp+12]
    mov si, word [bp+10]
    mov di, word [bp+8]
    rep movsb

.exit:
    mov sp, bp
    pop di
    pop si
    pop bp
    pop ax ; return address from stack
    add sp, 6
    push ax ; return address back to stack
    ret
    



DEFCODE 'BRANCH', 0, BRANCH
    lodsw
    shl ax, 1 ; multiply ax by word size
    add si, ax
NEXT

DEFCODE '0BRANCH', 0, ZEROBRANCH
    pop ax
    test ax,ax
    ; TODO
    ; either jump to explicit label within BRANCH
    ; or leave this in place, jumping to implicit 
    ; label "dcn_code_BRANCH"
    jz dcn_code_BRANCH
    lodsw ; top of stack wasn't 0 (didn't take the last jmp), skip offset
NEXT

; parse token from buffer at addr
; return pointer to start of token length of parsed token
; ( addr -- addr_or_null len_or_zero )

; FIXME FIXME FIXME FIXME NOTE NOTE NOTE NOTE
; FIXME FIXME FIXME FIXME NOTE NOTE NOTE NOTE
; FIXME - this needs to take maxlen as of buffer as param
; FIXME - this needs to take maxlen as of buffer as param

DEFCODE 'TOKEN', 0, TOKEN
    call _token
NEXT

; FIXME FIXME FIXME FIXME NOTE NOTE NOTE NOTE
; FIXME FIXME FIXME FIXME NOTE NOTE NOTE NOTE
; FIXME - this needs to take maxlen as of buffer as param
; FIXME - this needs to take maxlen as of buffer as param

_token:
    push bp
    mov bp, sp

    sub sp, 10 ; NUMBER_OF_LOCAL_WORDS * 2

    ; first, skip whitespace until non-ws char found
    mov ax, word [bp+4] ; param1
    push ax
    %define MAXIMUM_TOKEN_SEPARATING_WHITESPACE 50
    push word MAXIMUM_TOKEN_SEPARATING_WHITESPACE
    call _memnotws
    pop ax
    test ax,ax
    jz .token_not_found_exit

    ; first non-ws byte is at [ax] now
    mov word [LOCAL1], ax

    ; address of first non-whitespace char of possible token
    push ax

    ; FIXME - this might be a parameter instead of hardcoded
    %define MAXIMUM_TOKEN_LENGTH 32
    ; max length of possible token
    push word MAXIMUM_TOKEN_LENGTH

    call _memws

    pop ax
    test ax,ax
    jz .token_not_found_exit

    mov dx, word [LOCAL1] ; start of token without leading whitespace
    sub ax, dx
    ; ax now contains length of token
    mov word [LOCAL2], ax

.token_found_exit:
    mov bx, bp ; local parameters can be refered from bx backwards
    mov sp, bp
    pop bp
    pop ax ; return address to ax
    add sp, 2 ; remove params from stack
    mov dx, [bx-4] ; found token length
    push dx
    mov dx, [bx-2] ; found token start address
    push dx
    push ax
    ret

.token_not_found_exit:
    mov sp, bp
    pop bp
    pop bx ; return address
    add sp, 2 ; clear parameters
    xor ax,ax
    push ax
    push ax
    push bx ; return address
    ret



DEFCODE 'DONE', 0, DONE
BOCHS_BREAKPOINT
    jmp _done_loop
_done_text: db 'FrogFort stopped.',0xa,0xd,0x0
_done:
_done_loop:
    push si
    push word _done_text
    call _printsz
    pop si
    xor ax, ax
    int 0x16
    jmp _done_loop
NEXT ; note that this will not get called

align 2, db 0
START_OF_USER_DICT:
    db 'frog'


times 65535-($-$$) db 0xcc
