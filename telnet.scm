;
; TELNET Protocol utilities (RFC 854).
;

(module telnet

  (string-to-telnet
   telnet-to-list
   telnet-to-string
   get-next-string
   command?
   command-type
   command-code
   command-option
   command-raw-data
   incomplete-command?
   incomplete-command-raw-data)

  (import scheme chicken)
  (use data-structures srfi-1 srfi-13 defstruct)

  (define cmd-SE   (integer->char 240))
  (define cmd-NOP  (integer->char 241))
  (define cmd-DM   (integer->char 242))
  (define cmd-BRK  (integer->char 243))
  (define cmd-IP   (integer->char 244))
  (define cmd-AO   (integer->char 245))
  (define cmd-AYT  (integer->char 246))
  (define cmd-EC   (integer->char 247))
  (define cmd-EL   (integer->char 248))
  (define cmd-GA   (integer->char 249))
  (define cmd-SB   (integer->char 250))
  (define cmd-WILL (integer->char 251))
  (define cmd-WONT (integer->char 252))
  (define cmd-DO   (integer->char 253))
  (define cmd-DONT (integer->char 254))
  (define cmd-IAC  (integer->char 255))

  ;
  ; represents a complete telnet command block
  ;
  (defstruct command
             type     ; 'command, 'option-code, 'subnegotiation
             code     ; the command character
             option   ; the option character, only used when type = 'option-code
             raw-data ; the raw command block, as a string
             )

  ;
  ; represents an incomplete telnet command block
  ;
  (defstruct incomplete-command
             raw-data
             )

  ;------------------------------------------------------------
  ;
  ; In these procedures, we use the "telnet string" term to name the data format
  ; which is a string of bytes respecting the telnet protocol. For instance, in
  ; the "telnet string", a CR character is guaranteed to be always followed
  ; either by LF or NUL, it should never appear alone, according to the RFC 854:
  ;
  ; "...                         for the sake of consistency, the protocol
  ;      requires that a NUL be inserted following a CR not followed by
  ;      a LF in the data stream."
  ;
  ; Another character necessary to be transformed to a "telnet string" is the
  ; byte 255, that is the basic symbol in the Telnet protocol, representing the
  ; Telnet character IAC (Interpret As Command). This is the only Telnet
  ; character that needs to be escaped, and just doubling it does the job, like
  ; this:
  ;
  ; <... stream ...> <IAC IAC> <... stream ...>
  ;
  ; This will make Telnet understand <IAC IAC> as a literal byte 255, and
  ; therefore it will not "Interpret [the following bytes] As Command".
  ;
  ;------------------------------------------------------------

  ;
  ; Convert string STR to a "telnet string".
  ;
  ; STR can be any stream of bytes, the converted string will always respect
  ; the format defined in the Telnet protocol.
  ;
  ; The optional argument APPEND-NULL-TO-CR defaults to #t, which means the
  ; <CR> characters not followed by a <LF> wil have a <NUL> appended in front
  ; of them. #f will disable this behaviour, which might be good when
  ; transmitting binary data, probably useful with the TRANSMIT-BINARY option,
  ; described in RFC 856 "TELNET BINARY TRANSMISSION".
  ;
  (define (string-to-telnet str #!optional (append-null-to-cr #t))

    (let format ((current-char  (string-ref str 0))
                 (rest          (substring/shared str 1))
                 (telnet-string "") )

      ; if current-char is CR and next-char is not LF,
      ;     add to telnet-string the sequence <CR NUL>
      ; else if current-char is IAC
      ;     add to telnet-string the sequence <IAC IAC>
      ; else
      ;     add to telnet-string current-char
      (define append-to-telnet-string

        (cond
          ((and append-null-to-cr
                (char=? current-char #\return)
                (or (string-null? rest)
                    (not (char=? (string-ref rest 0) #\linefeed)) ))
           (string #\return #\nul) )

          ((char=? current-char cmd-IAC)
           (string cmd-IAC cmd-IAC) )

          (else
           (string current-char) )))

      (if (string-null? rest)

        (string-append telnet-string append-to-telnet-string)

        (format (string-ref rest 0)
                (substring/shared rest 1)
                (string-append telnet-string append-to-telnet-string) ))))

  ;
  ; Convert the Telnet string TELNET-STRING to a regular string, without the
  ; escaped data that is needed to go into a Telnet stream.
  ;
  ; The optional argument STRIP-NUL-AFTER-CR defaults to #t, which means that
  ; the <NUL> characters following <CR> characters will be stripped out. #f
  ; will disable this behaviour, which might be good when receiving binary
  ; data, probably useful with the TRANSMIT-BINARY option, described in RFC
  ; 856 "TELNET BINARY TRANSMISSION".
  ;
  (define (telnet-to-string telnet-string #!optional (strip-nul-after-cr #t))

    (let format ((current-char (string-ref telnet-string 0))
                 (rest         (substring/shared telnet-string 1))
                 (pure-string  "") )

      (cond
        ; last byte: append that byte and return string.
        ((string-null? rest)
         (string-append pure-string (string current-char)) )

        ; <CR><NUL>  : append <CR>  and bypass <NULL>
        ; <IAC><IAC> : append <IAC> and bypass the other <IAC>
        ((or (and strip-nul-after-cr
                  (char=? current-char #\return)
                  (char=? (string-ref rest 0) #\nul) )
             (and (char=? current-char cmd-IAC)
                  (char=? (string-ref rest 0) cmd-IAC) ))
         (if (= (string-length rest) 1)
             (string-append pure-string (string current-char))
             (format (string-ref rest 1)
                 (substring/shared rest 2)
                 (string-append pure-string (string current-char)) )))

        ; regular character: append it and iterate with next character
        (else
         (format (string-ref rest 0)
                 (substring/shared rest 1)
                 (string-append pure-string (string current-char)) )))))

  ;
  ; Convert the string <telnet-data> to a list of separated strings and commands.
  ;
  (define (telnet-to-list telnet-data)

    (let build-list ((block (get-next-block telnet-data))
                     (telnet-list   '()) )

      (define current-block (first block))
      (define rest (second block))

      (cond

        ; We're done, return the list.
        ((and (string-null? current-block) (string-null? rest))
         telnet-list )

        ; If it's an incomplete command. Since we can't have more blocks after
        ; an incomplete command, let's return from here.
        ((string-null? current-block)
         (let ((cmd (make-incomplete-command raw-data: rest)))
           (append telnet-list (list cmd)) ))

        ; If it's a Telnet command (<IAC> is the first byte).
        ((char=? (string-ref current-block 0) cmd-IAC)
         (let ((cmd (select (string-ref current-block 1)

                      ; Option code (3 bytes)
                      ((cmd-WILL cmd-WONT cmd-DO cmd-DONT)
                       (make-command type:   'option-code
                                     code:   (string-ref current-block 1)
                                     option: (string-ref current-block 2)
                                     raw-data: current-block ))

                      ; Subnegotiation
                      ((cmd-SB)
                       (make-command type:   'subnegotiation
                                     code:   (string-ref current-block 1)
                                     raw-data: current-block ))

                      ; Regular command (just 2 bytes: <IAC><something>)
                      (else
                       (make-command type:   'command
                                     code:   (string-ref current-block 1)
                                     raw-data: current-block )))))
           (build-list (get-next-block rest) (append telnet-list (list cmd))) ))

        ; It's a Telnet string.
        (else
         (build-list (get-next-block rest) (append telnet-list (list current-block))) ))))

  ;
  ; Return the next string in <telnet-list> (like the list returned by the
  ; procedure TELNET-TO-LIST) as a proper string, without the Telnet escaped
  ; bytes (doubled <IAC> or <NUL> after <CR>) in it, along with the remaining
  ; of the list.
  ;
  ; It returns this list:
  ;
  ; (STRING REST-OF-TELNET-LIST)
  ;
  ; #f will be returned if the first element of <telnet-list> is not a string.
  ;
  ; Note that if it is able to return the next string, then REST-OF-TELNET-LIST
  ; will be inevitably either an empty list or a list which first element
  ; is a command and this command is not <IAC><IAC>. This is because
  ; GET-NEXT-STRING tries to concatenate the longest sequence of consecutive
  ; strings, starting from the first element of the list.
  ;
  ; The parameter STRIP-NUL-AFTER-CR is the same used in TELNET-TO-STRING, and
  ; defaults to #t.
  ;
  (define (get-next-string telnet-list #!optional (strip-nul-after-cr #t))

    (let build-string ((telnet-list telnet-list)
                       (next-string #f         ) )

      (cond
        ((null? telnet-list)
         (if next-string
             (list (telnet-to-string next-string strip-nul-after-cr) telnet-list)
             #f ))

        (else
         (let ((curr-item (car telnet-list)))

           (cond
             ((string? curr-item)
              (build-string (cdr telnet-list)
                            (if next-string
                                (string-append next-string curr-item)
                                curr-item )))

             ; <IAC><IAC>
             ((and (command? curr-item)
                   (eq? (command-type curr-item) 'command)
                   (char=? (command-code curr-item) cmd-IAC) )
              (build-string (cdr telnet-list)
                            (if next-string
                                (string-append next-string (command-raw-data curr-item))
                                (command-raw-data curr-item) )))

             ; It's a Telnet command, stop.
             (else
               (if next-string
                   (list (telnet-to-string next-string strip-nul-after-cr) telnet-list)
                   #f ))))))))

  ;
  ; Return a list containing the first block of the string <telnet-data> as the
  ; first element and the remaining data as the second element, in this format:
  ;
  ;    '(NEXT-BLOCK REST-OF-TELNET-DATA)
  ;
  ; A "block" is:
  ;     - a Telnet String, that is, no telnet commands are in there; or
  ;     - a Telnet command (a string starting with <IAC>, including the escape
  ;       sequence <IAC><IAC>).
  ;
  ; If you concatenate NEXT-BLOCK and REST-OF-TELNET-DATA you should have the
  ; original data again.
  ;
  ; It's possible to get this result:
  ;
  ;    '("" REST-OF-TELNET-DATA)
  ;
  ; It means that the procedure wasn't able to parse the telnet data as a
  ; complete block, so it returned an empty block instead, and the complete
  ; string in the second element of the list. In this case REST-OF-TELNET-DATA
  ; is equal to <telnet-data>.
  ;
  ; Both strings in the returned list are raw data, they aren't modified in any
  ; way. This procedure is useful to "cut" a telnet data between strings and
  ; commands.
  ;
  ; Note 1: <IAC IAC> is a block by itself, one can interpret it as a
  ;   command that prints the databyte 255, so that is what we do here.
  ;   So it will be separated from the string where it is included. If you want
  ;   to get the full string, please remember to call GET-NEXT-BLOCK another
  ;   time after it return a string, so that you can know if the next block is
  ;   an <IAC IAC> or another command.
  ;
  ; Note 2: A subnegotiation will always be returned as a full block regardless
  ;   of <parameters> containing or not <IAC> characters, like this:
  ;
  ;     <IAC> <SB> <cmd> <parameters> <IAC> <SE>
  ;
  (define (get-next-block telnet-data)

    (let build-block ((first-block "")
                      (rest        telnet-data)
                      ; stores the current state:
                      ; 'in-string, 'in-iac, 'in-option, 'in-sb or 'in-sb-iac
                      (state       'in-string) )

        (if (string-null? rest)

          ; There's nothing more to add to first-block so we will return what we
          ; have so far.
          ;
          ; 'in-string is the initial state, and when we finish at that state
          ; we know that first-block is complete.
          ;
          ; When we are not in the 'in-string state, then we have an incomplete
          ; block (since we are in the middle of something). In this case we
          ; will return an empty block, and we will return the incomplete block
          ; as the rest of the string.
          (if (eq? state 'in-string)
            (list first-block "")
            (list "" first-block) )

          (let ((current-char (string-ref rest 0)))

            (cond

              ((eq? state 'in-string)
               (cond

                 ; When we find an IAC character, let's see if there is already
                 ; a non-empty first-block.
                 ;
                 ; If it's empty, we know we are starting to build a new block,
                 ; then we change the state to 'in-iac
                 ;
                 ; If it's not empty, that means we reached the end of our current
                 ; block, let's return all we have so far.
                 ((char=? current-char cmd-IAC)
                  (if (string-null? first-block)
                    (build-block (string current-char) (substring/shared rest 1) 'in-iac)
                    (list first-block rest) ))

                 (else
                  (build-block (conc first-block current-char) (substring/shared rest 1) 'in-string)) ))

              ((eq? state 'in-iac)
               (select current-char

                 ((cmd-WILL cmd-WONT cmd-DO cmd-DONT)
                  (build-block (conc first-block current-char) (substring/shared rest 1) 'in-option) )

                 ((cmd-SB)
                  (build-block (conc first-block current-char) (substring/shared rest 1) 'in-sb))

                 ; every other command is just 1 byte after <IAC>, so we can safely finish here
                 (else
                  (list (conc first-block current-char) (substring/shared rest 1)) )))

              ((eq? state 'in-option)
               ; An option is represented by just 1 byte after either
               ; <IAC><WILL>, <IAC><WONT>, <IAC><DO> or <IAC><DONT>.
               ; Therefore we can return directly.
               (list (conc first-block current-char) (substring/shared rest 1)) )

              ; When inside the SB command, we only will return the block when we reach the
              ; SE command, i.e., <IAC><SE>.
              ;
              ; We want to "understand"  <IAC><SB> until <IAC><SE> as a unique block, so
              ; that we will have another state to interpret <IAC> when inside the
              ; subnegotiation, called 'in-sb-iac .
              ;
              ; That way we can make sure that when we find <SE> in the 'in-sb-iac state,
              ; it's certainly the correct <SE> that ends the subnegotiation, and not
              ; a bogus one that might appear in the byte stream inside a subnegotiation,
              ; for instance, when there is <anything-but-IAC><IAC><IAC><SE>, that <SE>
              ; doesn't end the subnegotiation because the previous <IAC> is a literal
              ; byte, escaped by the former <IAC>.
              ((eq? state 'in-sb)
               (if (char=? current-char cmd-IAC)
                 (build-block (conc first-block current-char) (substring/shared rest 1) 'in-sb-iac)
                 (build-block (conc first-block current-char) (substring/shared rest 1) 'in-sb) ))

              ((eq? state 'in-sb-iac)
               (if (char=? current-char cmd-SE)
                 (list (conc first-block current-char) (substring/shared rest 1))
                 (build-block (conc first-block current-char) (substring/shared rest 1) 'in-sb) ))))))))
