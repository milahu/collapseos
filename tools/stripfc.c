#include <stdio.h>

/* read stdin and strip Forth-style comments before spitting in stdout. This
also deduplicate spaces and newlines.

THIS PARSING IS IMPERFECT. Only a Forth interpreter can reliably detect
comments. For example, a naive parser misinterprets the "(" word definition as
a comment.

We work around this by considering as a comment opener only "(" chars preceeded
by more than once space or by a newline. Hackish, but works.
*/

int main()
{
    int spccnt = 2; // if the first char is a (, consider it a comment opener.
    int incomment = 0;
    int c;
    c = getchar();
    while ( c != EOF ) {
        if (c == '\n') {
            if (!incomment) {
                // We still spit newlines whenever we see them, Forth interpreter
                // doesn't like when they're not there...
                putchar(c);
            }
            spccnt += 2;
        } else if (c == ' ') {
            spccnt++;
        } else {
            if (incomment) {
                if ((c == ')') && spccnt) {
                    incomment = 0;
                }
            } else {
                if ((c == '(') && (spccnt > 1)) {
                    putchar(' ');
                    spccnt = 0;
                    int next = getchar();
                    if (next <= ' ') {
                        incomment = 1;
                        continue;
                    }
                    putchar(c);
                    c = next;
                }
                if (spccnt) {
                    putchar(' ');
                }
                putchar(c);
            }
            spccnt = 0;
        }
        c = getchar();
    }
    return 0;
}
