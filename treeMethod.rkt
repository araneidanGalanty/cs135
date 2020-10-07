#lang racket

;;; Abandon hope all ye who enter here.

;;  This was made organically over the course of not a lot of time a million years ago.
;;  There might be some unused functions.
;;  Also, I now realize that commenting != actual memory recall.


;; Note: I added a few additional comments (minor stuff) for this presentaiton.

;;; INSTRUCTIONS
;; 0. Key:
;;     - "!"   : not
;;     - "&"   : and
;;     - "|"   : or
;;     - "->"  : implies
;;     - "<->" : biconditional
;;     I now realize that ".'." would have been cool to have as a symbol for "therefore"
;;   On Propositions:
;;     - Can be any string of letters (e.g. "a", "abcde", "AbCdE")
;;     - SEVERE ISSUE: Submitted version DOES NOT WORK with non single letter.
;;       This is fixed in this presentation version (I forgot one snippet of regex).
;;   On general input:
;;     - Can have any number of spaces anywhere, probably.
;;     - Your input can have extremely ugly amounts of parentheses.
;;       It doesn't matter as long as it's well formed.
;;     - As long as you don't put spaces inside propositions or operators, you're probably good.
;;     - Also, "!!!!...a" is not possible. I wish I implemented this but didn't think to til now.
;;       I think you can do "!(!a)", though. But don't quote me on that. My regex is a mess.
;;     - Why did I do this? Because I am lazy and don't want to propperly format everything all the time.
;; 1. Run program
;; 2. Follow prompts.
;; 3. Observe answer (list of true propositions in alphabetical order)

;; CLARIFICATION: Answer is a list of propositions that are assumed ALWAYS return true.
;;                so, "a" means "a" has the value #t. "!a" means "a" is #f

;; This program returns the first found contradiction.

;; Format.
;; (Operation Proposition Proposition)
;; Operations are strings. Propositions can be lists or single-proposition strings (not compound)
;; Examples:
;;  - ("!" "B")
;;  - ("->" "ABC" "DEF")
;;  - ("&" ("!" A) ("|" "B" ("&" "A" "B")))

;; Why prefix? Because calling "last" felt awkward, and some logic later that I forgot about.


;; I just realized I mispelled this. And never used it :(
(define no-coutner "No Counterexample")

;; Very poor form with these global "variables" using set!
;; At some point, I really should refactor functions that use these.


;; Proposition stack for individual proposition parsing.
(define single-prop-stack '())

;; Stack of operators for individual proposition parsing.
(define infix-op-stack '())

;; NOTES:
;; X V Y, where X, Y are any propositions => branch!
;; X A Y, where X,Y are any proposition => add to stack...?
;; ??? notation
;; Why use set! ? Becuase maximum lazy and insanity.
;; Wasn't sure what was gonna need to be global or local.

;; FORMATTING:
;; Propositions are strings with lowercase letters.

;; A compound proposition is a list of propositions and operators in RPN.

;; A stack is a list such that the top of the stack is the first element,
;; the last element is the end of the stack.
;; Why? Because I'm too lazy to pop stuff some other way than (cdr stack)


;; Convert input to RPN. Then to postfix. Lot of helper functions for that below


;; Turn input infix string into list of operators and propositions
;; Seperator is spaces of any length, or nonspace followed by operator,
;; or operator followed by string of letters.
;; (split-infix "a<->b")
(define (split-infix input)
  (string-split input
                #rx"[ ]+|((?<=[(])(?=.))|((?<=[^ <])((?=[!&|])|(?=(->))|(?=(<->))))|((?<=[!&|])|(?<=(->))|(?<=(<->))(?=[a-zA-Z]))|(((?<=!)|(?<=(->)))(?=[(]))|((?<=[a-zA-Z]|[)])(?=[)]))|((?<=[(])(?=[a-zA-Z]))"))
;; SUPER UGLY REGEX. Matches:
;; - parenthesis followed by parenthesis
;; - any sequence of spaces
;; - where operators follow spaces
;; - where letters follow operators
;; - where ! or -> precede parenthesis (split betweeen op and parenthesis)
;; - where letters or ")" precede ")" (split betweeen letters and parenthesis)
;; - maybe something past me forgot to list
;; I think I tried to put this on multiple lines for clarity, but then it didn't work. I think.


;; Get precedence of operator.
;; Why not make lower numbers indicate higher precedence?
;; Because this guide to the algo https://www.youtube.com/watch?v=HJOnJU77EUs
;; Yes. Peak lazy. Didn't convert to the opposite.
(define (get-precedence op)
  (cond ((regexp-match? #rx"!" op) 5)
        ((regexp-match? #rx"&" op) 4)
        ((regexp-match? #rx"[|]" op) 3)
        ((regexp-match? #rx"<->" op) 1)
        ((regexp-match? #rx"->" op) 2)
        ((regexp-match? #rx"[)]" op) 6)
        (else 0) ; open parenthesis
        ))

;; SHUNTING YARD STUFF BELOW

;; Push onto operator stack
(define (push-ios  e)
  (begin (set! infix-op-stack (cons e infix-op-stack)) e))

;; Pop operator stack
(define (pop-ios S)
  (let ([e (car infix-op-stack)]) (begin (set! infix-op-stack (cdr infix-op-stack)) e)))

;; Pops stack members off until an open parenthesis. Returns stack of pops
(define (close-paren stack)
  (if (equal? (car stack) "(")
      (begin (set! infix-op-stack (cdr infix-op-stack))
             '())
      (cons (pop-ios stack)
            (close-paren (cdr stack)))))

;; Pops remaining stack members off. Returns stack of pops
(define (end-pop stack)
  (if (null? stack)
      '()
      (cons (pop-ios stack)
            (end-pop (cdr stack)))))

;; Converts infix list to postfix list via shunting yard algo
(define (inlist-to-postlist input)
  ;; Termination: pop remaining ops onto answer
  (cond ((null? input) (end-pop infix-op-stack))
        ;; proposition. add to answer list
        ((regexp-match? #rx"[a-zA-Z]+" (car input)) (cons (car input) (inlist-to-postlist (cdr input))))
        ;; Close parentehesis. Pop ops until open parenthesis. Then pop open parenthesis.
        ;; Continue from then on.
        ((eq? 6 (get-precedence (car input)))
         (append (close-paren infix-op-stack) (inlist-to-postlist (cdr input))))
        ;; OP added to empty op stack. push op to stack.
        ((null? infix-op-stack) (begin (push-ios (car input)))
                                       (inlist-to-postlist (cdr input))
                                       )
        ;; open parenthesis. push to stack.
        ((eq? 0 (get-precedence (car input))) (begin (push-ios (car input)))
                                              (inlist-to-postlist (cdr input))
                                              )
        ;; greater precedence op push to stack.
        ((< (get-precedence (car infix-op-stack)) (get-precedence (car input)))
         (begin (push-ios (car input))
                (inlist-to-postlist (cdr input))
                ))
        ;; Less or equal precedence. Pop op stack to answer.
        ;; Repeat til you can add op to stack.
        (else (let ([e (car infix-op-stack)])
                (begin (pop-ios infix-op-stack)
                       (cons e (inlist-to-postlist input)))))
        ))


;; random test for next function
;; This is outdated.
(define parse-test-input '("a" "b" "!" "&" "c" "d" "|" ">"))
;; expected: '("^" ("&" "a" ("!" "b")) ("|" "c" "d"))


;; Really crappy parse function. should return (operator A B), where A and B are propositions
;; Assumes RPN input. relies on parse-call (a recursive RPN parser)
;; Purpose: Convert split RPN into a prepared proposition
;; Why not just reverse? Because I need to have something in the form (operator A B) for easier
;; processing later. probably.
;; Would reversing then getting the form (operator A B) have been easier?
;; Maybe? It's 3:10 AM, so that's a question for future me.
(define (parse-rpn-list prop)
  (begin (parse-call prop)
         single-prop-stack))

(define (parse-call prop)
  ;; Terminate if no more characters to process
  (cond ((null? prop) (set! single-prop-stack (car single-prop-stack)))
        ;; Propositions are added to the stack
        ;; Uh, I forgot to check A-Z and strings longer than length 1 in the submitted code.
        ((regexp-match? #rx"[a-zA-Z]+" (car prop))
         (begin
           (set! single-prop-stack (cons (car prop) single-prop-stack))
           (parse-call (cdr prop))))
        ;; Not operator pops first proposition off stack
        ;; , then gets paired with the proposition in a list.
        ((regexp-match? #rx"!" (car prop))
         (begin
           (set! single-prop-stack (cons (list (car prop) (car single-prop-stack)) (cdr single-prop-stack)))
           (parse-call (cdr prop))))
        ;; Other oprators take two propositions off the stack
        ;; , then get put with both propositions in a list.
        (else (begin
                (set! single-prop-stack (cons (list (car prop) (cadr single-prop-stack) (car single-prop-stack)) (cddr single-prop-stack)))
                (parse-call (cdr prop))))
        ))

;; Putting the parsing process all together
;; Takes an infix string and converts it to a single big compound proposition in postfix.
;; String -> ("operator" proposition proposition)
(define (infix-parse-to-prefix string) (parse-rpn-list (inlist-to-postlist (split-infix string))))

;;(infix-parse-to-prefix "!(a&b)->!(c|d)")

;; DONE!
;; - Take input propositions into a stack, negating the conclusion.
;; - Map parser to every proposition.
;; - Figure out the actual tree method.

;; Splits argument into a list of propositions
(define (split-argument input)
  (string-split input
                #rx"[ ]*(,)[ ]*"))

;; Negate the conclusion. TODO: Make faster. Maybe better input.
;; Expects non-empty input

(define (negate-conclusion split-input)
  (cond ((null? split-input) (error "negate-conclusion: invalid argument"))
        ((null? (cdr split-input))
         (list (string-append "!(" (car split-input) ")")))
        
        (else (cons (car split-input) (negate-conclusion (cdr split-input))))))

;; Maps prepared prefix notation to every proposition.

(define (parse-argument input-string)
  (map infix-parse-to-prefix (negate-conclusion (split-argument input-string))))

;; Theory:
;; Recursive call using left side.
;; Call using right side.
;; Somehow pop off stuff when done with them? Maybe by placing pop last.


;; WHEN ADDING TO ANS STACK.
;; Form: ("op/blank" "key")

;;

;; Notes:
;; ("&" "a" "b")
;; ("&" ("!" "a") "b")

;; uh, remove redundancy later.

;; These two cases just strip the operator from a list,
;; so the two props can be processed.

(define (and-case prop)
  (cdr prop))

(define (or-case prop)
  (cdr prop))

(define (implies-case prop)
  (list "|" (list "!" (cadr prop)) (caddr prop)))

(define (iff-case prop)
  (list "&" (list "->" (cadr prop) (caddr prop)) (list "->" (caddr prop) (cadr prop))))

;; DeMorgan
;; Note: (x (x x))

(define (demorgan prop)
  (cond ((equal? "&" (caadr prop))
         (list "|" (list "!" (cadadr prop)) (list "!" (caddr (cadr prop)))))
        ((equal? "|" (caadr prop))
         (list "&" (list "!" (cadadr prop)) (list "!" (caddr (cadr prop)))))
        ((equal? "->" (caadr prop))
         (demorgan (list "!"(implies-case (cadr prop)))))
        (else (cadadr prop))
      ))

;; Take input
;; If there is an and statement of compound propositions, add them to u-stack.
;; Else, if containing !prop and/or prop, add both to ans-stack.
;; ans-stack push must check for duplicates. Change global state variable.
;;


;; Known: ((propositional keys) (propositions))
;; Checks for key copies. Then checks for duped proposition
(define (contr? a-stack)
  (contr-helper a-stack '(() ())))

;; known should be a list of two empty lists
(define (contr-helper a-stack known)
  (cond ((null? a-stack) #f)
        ((null? (car a-stack)) (contr-helper (cdr a-stack) known))
        ;; grab second element. should the the prop letter.
        ;; case: new a-stack prop is in known key.
        ((member (cadar a-stack) (car known))
         ;; if not member of the known prop list, but we have prop. we have contradiction.
         (if (not (member (car a-stack) (cadr known)))
             #t
             (contr-helper (cdr a-stack) (list
                                           (car known)
                                           (cons (car a-stack) (cadr known))))))
        (else (contr-helper (cdr a-stack) (list
                                           (cons (cadar a-stack) (car known))
                                           (cons (car a-stack) (cadr known)))))
        ))

;; Single prop
;; "a"
;; ("!" "a")

(define (single-prop? prop)
  ;; Not a list? No processing needed
  (if (list? prop)
      ;; Length 2? Otherwise, compound prop.
      (if (equal? 2 (length prop))
          ;; Not statement with sole prop?
          ;; Otherwise, compount (or !!a, but whatever)
          (if (and (equal? "!" (car prop)) (not (list? (cadr prop))))
              #t
              #f)
          #f)
      #t
      ))


;; uhg. had to put these here for filtering functions.
(define (not-null? x)
  (not (null? x)))
(define (not-single-prop? x)
  (not (single-prop? x)))

;; u-stack is "unprocessed" or "unchecked" stack. a-stack is the "answer" stack.
;; Why stacks? So I can easily close branches to move up the stacks.
;; If you need to remove a contradiction, we just go the the previous call's memorized stack.
(define (tree-method u-stack a-stack)
  (cond ((and (null? u-stack) (not (contr? a-stack))) a-stack)
        ((null? u-stack) #f)
        ;; null on u-stack
        ;; pop and continue
        ((null? (car u-stack)) (tree-method (cdr u-stack) a-stack))
        ;; single prop
        ((single-prop? (car u-stack))
         (let ([new-ans "placeholder"])
           ;; format single prop
           (set! new-ans (if (list? (car u-stack))
                                         (car u-stack)
                                         (list "" (car u-stack))))
           ;; if contradictory, kill branch
           (if (contr? (cons new-ans a-stack))
                 #f
                 ;; else, push to answer stack
                 (tree-method (cdr u-stack) (append (list new-ans) a-stack)))))
        ;; case: not
        ((equal? "!" (caar u-stack))
         (let ([newprop (car u-stack)] [new-ans '()])
           ;; Start processing
           (begin
             ;; Apply demorgans
             (set! newprop (demorgan newprop))
             ;; First item is single prop.
             ;; Format and add to new possible answer
             (if (single-prop? newprop)
                 (begin (set! new-ans (cons (if (list? newprop)
                                         newprop
                                         (list "" newprop)) new-ans))
                        (set! newprop '()))
                 #f)
             ;; remove nulls. Some of these are redundant, probably, somewhere.
             (set! new-ans (filter not-null? new-ans))
             ;; remove single props from the compound proposition list.
             ;; maybe this isn't necessary.
             (if (not-null? new-ans)
                 (set! newprop '())
                 #f)
             ;; If there is a contradiction, kill this branch. Else, continue.
             ;; pop off processed proposition and push new proposition.
             ;; Push answers to answer stack.
             (if (contr? (append new-ans a-stack))
                 #f
                 
                 (tree-method (cons newprop (cdr u-stack)) (append new-ans a-stack)))
             )))
        ;; case: implies
        ((equal? "->" (caar u-stack))
         (tree-method (cons (implies-case (car u-stack)) (cdr u-stack)) a-stack))
        ;; case: IFF
        ((equal? "<->" (caar u-stack))
         (tree-method (cons (iff-case (car u-stack)) (cdr u-stack)) a-stack))
        ;; case: and
        ((equal? "&" (caar u-stack))
         (let ([newprops (and-case (car u-stack))] [new-ans '()])
           (begin
             (set! newprops (filter not-null? newprops))
             ;; First item is single prop
             (if (single-prop? (car newprops))
                 ;; Apply some formatting to the proposition.
                 ;; Must be form ("op/no op" "key")
                 (set! new-ans (cons (if (list? (car newprops))
                                         (car newprops)
                                         (list "" (car newprops))) new-ans))
                 #f)
             ;; Second item single prop
             ;; formatting
             (if (single-prop? (cadr newprops))
                 (set! new-ans (cons (if (list? (cadr newprops))
                                         (cadr newprops)
                                         (list "" (cadr newprops))) new-ans))
                 #f)
             ;; Set single props as propper
             (set! new-ans (filter not-null? new-ans))
             ;; ensure newprops are not single props to prevent double counting.
             ;; Could probably just handle single-props with the u-stack, but I made this already
             ;; and don't want anything to explode.
             (set! newprops (filter not-single-prop? newprops))
             ;; length 2 on addition to answer stack means only single props.
             (if (equal? 2 (length new-ans))
                 ;; if single props are the same, just take one. else. contr! handled later
                 (if (equal? (car new-ans) (cadr new-ans))
                     (set! new-ans (cdr new-ans))
                     #f)
                 #f)
             ;; check for contradictions. kill if contr. else, add new compount props and modify answer stack.
             (if (contr? (append new-ans a-stack))
                 #f
                 (tree-method (append newprops (cdr u-stack)) (append (filter not-null? new-ans) a-stack)))
             ))) ;; and case end
        ;; case: or
        ;; Like and, but with two calls in places.
        ((equal? "|" (caar u-stack))
         (let ([newprops (or-case (car u-stack))] [new-ans '()] [pos-ans '()])
           (begin
             (set! newprops (filter not-null? newprops))
             ;; First item is single prop
             (if (single-prop? (car newprops))
                 ;; Apply some formatting to the proposition.
                 ;; Must be form ("op/no op" "key")
                 (set! new-ans (cons (if (list? (car newprops))
                                         (car newprops)
                                         (list "" (car newprops))) new-ans))
                 #f)
             ;; Second item single prop
             (if (single-prop? (cadr newprops))
                 (set! new-ans (cons (if (list? (cadr newprops))
                                         (cadr newprops)
                                         (list "" (cadr newprops))) new-ans))
                 #f)
             ;; Set single props as propper
             (set! new-ans (filter not-null? new-ans))
             (set! newprops (filter not-null? (filter not-single-prop? newprops)))
             (if (equal? 2 (length new-ans))
                 (if (equal? (car new-ans) (cadr new-ans))
                     (set! new-ans (cdr new-ans))
                     #f)
                 #f)
             ;; cases. Messy, but allows for branching with all these begin statements.
             (cond ((null? new-ans)
                    ;; No single props
                    (begin
                      ;; branch with first newprop
                      (set! pos-ans (tree-method (cons (car newprops) (cdr u-stack)) a-stack))
                      ;; If the first branch dies, try with another branch and return the result.
                      ;; this kills the branch or returns a counterexample.
                      ;; else newprop leads to dead branch, return false to kill the branch.
                      (if (false? pos-ans)
                          (begin (set! pos-ans (tree-method (cons (cadr newprops) (cdr u-stack)) a-stack))
                                pos-ans)
                          pos-ans))
                    )
                   ;; 1 single prop
                   ((equal? 1 (length new-ans))
                    (if (contr? (append new-ans a-stack))
                        ;; single prop contr. test the newprop branch instead.
                        (begin (set! pos-ans (tree-method (cons (car newprops) (cdr u-stack)) a-stack))
                               pos-ans)
                        ;; no contr.
                        ;; branch with the single-prop, then if that branch dies, test the newprop.
                        ;; return result.
                        (begin (set! pos-ans (tree-method (cdr u-stack) (append new-ans a-stack)))
                               (if (false? pos-ans)
                                   (begin (set! pos-ans (tree-method (cons (car newprops) (cdr u-stack)) a-stack))
                                          pos-ans)
                                   pos-ans)))
                    )
                   ;; two single props
                   (else (if (contr? (cons (car new-ans) a-stack))
                             ;; prop 1 contradiction.
                             (if (contr? (cons (cadr new-ans) a-stack))
                                 ;; contr 1,2. kill both branches
                                 #f
                                 ;; contr 1 only. test with branching via prop 2.
                                 (begin (set! pos-ans (tree-method (cdr u-stack) (cons (cadr new-ans) a-stack)))
                                        pos-ans)
                                 )
                             ;; no contr w/ prop 1
                             (if (contr? (cons (cadr new-ans) a-stack))
                                 ;; contr 2 only. test with branching via prop 1 only.
                                 (begin (set! pos-ans (tree-method (cdr u-stack) (cons (car new-ans) a-stack)))
                                        pos-ans)
                                 ;; no contrs
                                 ;; test with prop 1. If that branch dies, test with prop 2.
                                 ;; kill branches by returning pos-ans, or if branch lives, pos-ans returns counterex.
                                 (begin (set! pos-ans (tree-method (cdr u-stack) (cons (car new-ans) a-stack)))
                                        (if (false? pos-ans)
                                            (begin (set! pos-ans (tree-method (cdr u-stack) (cons (cadr new-ans) a-stack)))
                                                   pos-ans)
                                            pos-ans))
                                 ))
                             ))
             ))) ;; or case end
        ))

;; "a->(b->c),!c->!b&d,d->a|!c,!a->(d->b)"

;; Get proposition keys (letter strings).
;; This is important for sorting output without checking if strings are single or with a "!"


;; Checks if x is made of letters
(define (is-alpha? x)
  (regexp-match? #rx"[a-zA-Z]+" x))

;; Removes not-letter strings from x
(define (remove-non-alpha x)
  (filter is-alpha? x))

;; Gets proposition keys from an argument
(define (get-keys argument)
  (remove-duplicates (apply append (map remove-non-alpha (map split-infix (split-argument argument))))))

;; Gets second element of a list
(define (get-second x)
  (cadr x))

;; Keys proposition keys from a tree-method answer
(define (get-answer-keys ans)
  (map get-second ans))

;; Formats strings into answer format
(define (format-key x)
  (list "" x))

;; from euler lab
;; rmoves duplicates from a list
(define (remove-dupes L R)
  (define (recurse acc L)
    (if (null? L) (reverse acc)
        (recurse (if (member (car L) R)
                     acc
                     (cons (car L) acc))
                 (cdr L))))
  (recurse '() L))

;; Adds any missing propositions to an answer
(define (add-missing-props arg ans)
  (append ans (map format-key (remove-dupes (get-keys arg) (get-answer-keys ans)))))

;; Combines the first element of a list and the second into one string, assuming both are strings.
(define (smush string-tuple)
  (string-append (car string-tuple) (cadr string-tuple)))

;; Formats an answer string into a nicer printable statement.
(define (format-answer ans)
  (remove-duplicates (map smush ans)))

;; Get's the proposition key from a proposition string
(define (get-key-from-string x)
  (if (equal? "!" (substring x 0 1))
      (substring x 1)
      x))

;; Gives answer to a tree method in a nice format.
(define (give-answer arg ans)
  (if (false? ans)
      "Argument is Valid!"
      (sort (format-answer (add-missing-props arg ans)) #:key get-key-from-string string<?)))

;; "a->(b->c),!c->!b&d,d->a|!c,!a->(d->b)"
;; something something. Main loop
(define main-program
  (let loop ([argument ""][answer ""] [ip (open-input-string "")])
    (begin
      (display
       "Input argument as a list of compound propositions seprated by commas.
The final proposition in the list must be the conclusion.
Propositions are any combination of any letters, and may have operators.
Valid operators include (\"!\",\"&\",\"|\",\"->\",\"<->\")
Answer will be formatted as a list of true propositions,
or will state if the argument is valid
WARNING! Input must be well-formed:\n")
      (set! argument (read-line))
      (display (give-answer argument (tree-method (parse-argument argument) '())))
      (display "\n")
      (begin (display "Do you want to test another argument? (y/n)\n")
             (set! ip (read-line))
             (cond
               ((or (equal? ip "y") (equal? ip "Y")))
               (else (begin
                       (display "Exiting program....\n")
                       (exit)))))
      (loop "" "" "")
    )
    )
  
  )

;; Unfortunately, the, examples, here&othersToo, are, !in, my, submission

;; B -> I, M -> !D, I -> D, B -> !M

;; a->( b->c ),!c->!b&d ,d->a|!c,!a->   (((d->b)))

;; E -> OP,E -> ON,E -> B,P -> (K -> (!Z -> !B)),!P -> !OP,ON -> (X -> K),Z -> !X,X,!E