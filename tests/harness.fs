( Forth testing harness
  "#" means "assert". We stop at first failure, indicating
  the failure through IO on port 1 )

: fail SPC> ." failed" NL> 1 1 PC! BYE ;

: # IF SPC> ." pass" NL> ELSE fail THEN ;

: #eq 2DUP SWAP . SPC> '=' EMIT SPC> . '?' EMIT = # ;
