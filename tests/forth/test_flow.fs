: foo
    CASE
        'X' OF 42 ENDOF
        0x12 OF 43 ENDOF
        255 OF 44 ENDOF
        1+
    ENDCASE
;

'X' foo 42 #eq
0x12 foo 43 #eq
255 foo 44 #eq
254 foo 255 #eq
'S S0 #eq
