/** Header file generated with fdesign on Sun Nov 20 20:21:02 2005.**/

#ifndef FD_passphrase_h_
#define FD_passphrase_h_

/** Callbacks, globals and object handlers **/
extern void accept_func(FL_OBJECT *, long);
extern void reset_password(FL_OBJECT *, long);


/**** Forms and Objects ****/
extern FL_FORM *passphrase;

extern FL_OBJECT
        *ssh_prompt,
        *password,
        *cancel,
        *reset,
        *accept,
        *prompt,
        *reset,
        *cancel,
        *reset,
        *accept;


/**** Creation Routine ****/
extern void create_the_forms(void);

#endif /* FD_passphrase_h_ */
