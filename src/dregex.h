#ifndef DREGEX_H
#define DREGEX_H

#if DM_ENABLE_REGEX
P op_regex(void);
P op_regexi(void);

#define REGEX_BADPAT   (REGEX_ERRS+0) /* Invalid regular expression */
#define REGEX_ECOLLATE (REGEX_ERRS+1) /* Invalid collating element */
#define REGEX_ECTYPE   (REGEX_ERRS+2) /* Invalid character class */
#define REGEX_EESCAPE  (REGEX_ERRS+3) /* `\' applied to unescapable character */
#define REGEX_ESUBREG  (REGEX_ERRS+4) /* invalid backreference number */
#define REGEX_EBRACK   (REGEX_ERRS+5) /* brackets `[]' not balanced*/
#define REGEX_EPAREN   (REGEX_ERRS+6) /* paranthesis `()' not balanced */
#define REGEX_EBRACE   (REGEX_ERRS+7) /* braces `{}' not balanced */
#define REGEX_BADBR    (REGEX_ERRS+8) /* invalid repetition count(s) in `{}' */
#define REGEX_ERANGE   (REGEX_ERRS+9) /* invalid character rangin in `[]' */
#define REGEX_ESPACE   (REGEX_ERRS+10) /* ran out of memory */
#define REGEX_BADRPT   (REGEX_ERRS+11) /* `?', `*', or `+' operand invalid */
#define REGEX_UNKNOWN  (REGEX_ERRS+12) /* Unknown error */

#endif //DM_ENABLE_REGEX

#endif //DREGEX_H

