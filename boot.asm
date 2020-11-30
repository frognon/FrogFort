bits 16
org 0x7c00
%include "errcodes.asm"

jmp _bootloader_start


DBT_t:
    dbt_step_rate_time: db 0
    dbt_head_load_time: db 0
    dbt_timer_ticks_before_shutdown: db 0
    dbt_bytes_per_sector_code: db 0
    dbt_sectors_per_track: db 0
    dbt_inner_block_gap_between_sectors: db 0
    dbt_data_length: db 0
    dbt_fill_byte_for_formatted_sectors: db 0
    dbt_head_settle_time_milliseconds: db 0
    dbt_motor_startup_in_eights_of_second: db 0


boot_drive_num: db 0xff



_bootloader_start:
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7c00
    mov [boot_drive_num], dl

; TODO - get drive parameters (int 13,8)
    ;mov ah, 8
    ;mov dl, [boot_drive_num]
    


_boot:
; fnaa
    call _reset_boot_drive
    mov ax, 0x1000
    push es
    mov es, ax

    ;mov bx, 0x7e00 ; es:bx is start of buffer
    xor bx,bx  ; es:bx is start of buffer
    mov ah, 2 ; read disk sectors
    mov al, 0x40 ; number of sectors to load
    ;mov al, 128 ; number of sectors to load
    mov ch, 0 ; cylinder number, starts at 0
    mov cl, 2 ; start sector number, starts at sector 1, which is this initial bootloader
    mov dh, 0 ; head number, starts at 0
    mov dl, [boot_drive_num]
    int 0x13
    pop es

    jc _xxx_fail

_xxxxxx:
    xor ax,ax
    call _pstatus
    call _printnewline
    ;xor ax,ax 
    ;int 0x16
    jmp 0x1000:0000
    ; TODO : jmp far 0x1000:0000

_xxx_fail:
    mov ax, ERR_DISK_READ_FAILURE
    call _perror

    xor ax,ax
    int 0x16
    jmp _boot
; fnaa

_reset_boot_drive:
    xor ax,ax
    mov dh, 0
    mov dl, [boot_drive_num]
    int 0x13

    mov si, msg_disk_reset_success
    jnc _reset_boot_drive_exit

    mov si, msg_disk_reset_failure
    ; fallthrough
_reset_boot_drive_exit:
    call _printstr
    ret

_printstr:
    mov bp, sp
    mov ax,0x0e00
__printstr_1:
    lodsb
    cmp al, 0
    je __printstr_end
    int 0x10
    jmp __printstr_1
__printstr_end:
    ret

_print_al_as_hex:
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
    mov ax,0x0e0a
    int 0x10
    mov al, 0x0d
    int 0x10
    ret

_pstatus:
    push ax,
    mov si, msg_status
    call _printstr
    pop ax
    call _print_ax_as_hex
    call _printnewline
    ret

_perror:
    push ax ; error code in ax, save, print error message prefix
    mov si, msg_perror
    call _printstr
    pop ax ; restore error code
    call _print_ax_as_hex
    call _printnewline
    ret


msg_disk_reset_failure: db 'disk reset failure!',0xa,0xd, 0
msg_disk_reset_success: db 'disk reset success!',0xa,0xd, 0

msg_disk_read_success: db 'disk read success!',0xa,0xd, 0
msg_disk_read_failure: db 'disk read failure!',0xa,0xd, 0

msg_perror: db 'errcode:0x', 0
msg_status: db 'status:0x', 0
hexchars: db '0123456789abcdef'


_bootloader_die:
    xor ax,ax
    int 0x16
    jmp _bootloader_die

times 510-($-$$) db 0
dw 0xaa55
