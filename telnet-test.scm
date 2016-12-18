(require-library telnet)
(use test srfi-1 (prefix telnet telnet:))

(define test-str (conc "this is a test string using the "
                       #\xFF "character" #\return
                       "and other characters that resemble Telnet commands, like:" #\return
                       #\xFF #\xFE ", " #\xFF #\xFF #\xFA " and so on." #\return #\linefeed))

(define test-str-telnet (conc "this is a test string using the "
                       #\xFF #\xFF "character" #\return #\nul
                       "and other characters that resemble Telnet commands, like:" #\return #\nul
                       #\xFF #\xFF #\xFE ", " #\xFF #\xFF #\xFF #\xFF #\xFA " and so on." #\return #\linefeed))

;                                                  <IAC> <DONT><IAC> <string...>
(define test-str-telnet-cmd1 (conc test-str-telnet #\xFF #\xFE #\xFF #\xFA "and so on..."))
; Incomplete command at the end
;                                                  <IAC> <SB> ...
(define test-str-telnet-cmd2 (conc test-str-telnet #\xFF #\xFA #\xFF #\xFE "and so on..."))

(test test-str-telnet (telnet:string-to-telnet test-str))
(test test-str        (telnet:telnet-to-string test-str-telnet))

(test (conc "abc" #\return #\nul #\nul #\xFF #\xFF)
      (telnet:string-to-telnet (conc "abc" #\return #\nul #\xFF) #t))
(test (conc "abc" #\return #\xFF)
      (telnet:telnet-to-string (conc "abc" #\return #\nul #\xFF #\xFF) #t))
(test (conc "abc" #\return #\nul #\xFF #\xFF)
      (telnet:string-to-telnet (conc "abc" #\return #\nul #\xFF) #f))
(test (conc "abc" #\return #\nul #\xFF)
      (telnet:telnet-to-string (conc "abc" #\return #\nul #\xFF #\xFF) #f))

;
; telnet-list should be a list containing 8 elements:
; [string, command, string, command, string, command, command and string]
;
(test-group "telnet-list"
  (define telnet-list (telnet:telnet-to-list test-str-telnet))

  (test 8 (length telnet-list))
  (test #t (string?         (list-ref telnet-list 0)))
  (test #t (telnet:command? (list-ref telnet-list 1)))
  (test #t (string?         (list-ref telnet-list 2)))
  (test #t (telnet:command? (list-ref telnet-list 3)))
  (test #t (string?         (list-ref telnet-list 4)))
  (test #t (telnet:command? (list-ref telnet-list 5)))
  (test #t (telnet:command? (list-ref telnet-list 6)))
  (test #t (string?         (list-ref telnet-list 7))) )

;
; telnet-list1 should be a list containing 10 elements:
; [string, command, string, command, string, command, command, string, command and string]
;
(test-group "telnet-list1"
  (define telnet-list1 (telnet:telnet-to-list test-str-telnet-cmd1))

  (test 10 (length telnet-list1))
  (test #t (string?         (list-ref telnet-list1 0)))
  (test #t (telnet:command? (list-ref telnet-list1 1)))
  (test #t (string?         (list-ref telnet-list1 2)))
  (test #t (telnet:command? (list-ref telnet-list1 3)))
  (test #t (string?         (list-ref telnet-list1 4)))
  (test #t (telnet:command? (list-ref telnet-list1 5)))
  (test #t (telnet:command? (list-ref telnet-list1 6)))
  (test #t (string?         (list-ref telnet-list1 7)))
  (test #t (telnet:command? (list-ref telnet-list1 8)))
  (test #t (string?         (list-ref telnet-list1 9))) )

;
; telnet-list2 should be a list containing 9 elements:
; [string, command, string, command, string, command, command, string, incomplete-command]
;
(test-group "telnet-list2"
  (define telnet-list2 (telnet:telnet-to-list test-str-telnet-cmd2))

  (test 9 (length telnet-list2))
  (test #t (string?                    (list-ref telnet-list2 0)))
  (test #t (telnet:command?            (list-ref telnet-list2 1)))
  (test #t (string?                    (list-ref telnet-list2 2)))
  (test #t (telnet:command?            (list-ref telnet-list2 3)))
  (test #t (string?                    (list-ref telnet-list2 4)))
  (test #t (telnet:command?            (list-ref telnet-list2 5)))
  (test #t (telnet:command?            (list-ref telnet-list2 6)))
  (test #t (string?                    (list-ref telnet-list2 7)))
  (test #t (telnet:incomplete-command? (list-ref telnet-list2 8))) )

(test-group "get-next-string with test-str-telnet"
  (define telnet-list (telnet:telnet-to-list test-str-telnet))
  (define str-list (telnet:get-next-string telnet-list))

  (test test-str (car str-list))
  (test 0 (length (cadr str-list)))
  (test #f (telnet:get-next-string (cdr str-list))) )

(test-group "get-next-string with test-str-telnet-cmd1"
  (define telnet-list (telnet:telnet-to-list test-str-telnet-cmd1))
  (define str-list (telnet:get-next-string telnet-list))

  (test test-str (car str-list))

  (define str-list-2 (cadr str-list))
  (test 2 (length str-list-2))
  (test #f (telnet:get-next-string str-list-2))
  (test #t (telnet:command? (car str-list-2)))
  (test 1 (length (cdr str-list-2)))

  (define new-str-list (telnet:get-next-string (cdr str-list-2)))
  (test #t (string? (first new-str-list)))
  (test #t (null? (second new-str-list))) )


(test-group "get-next-string with test-str-telnet-cmd2"
  (define telnet-list (telnet:telnet-to-list test-str-telnet-cmd2))
  (define str-list (telnet:get-next-string telnet-list))

  (test test-str (car str-list))

  (define str-list-2 (cadr str-list))
  (test 1 (length str-list-2))
  (test #f (telnet:get-next-string str-list-2))
  (test #t (telnet:incomplete-command? (car str-list-2))) )
