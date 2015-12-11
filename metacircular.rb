#| Structure and Interpretation of Computer Programs                   |#
#|                                                                     |#
#|   Creative Commons Attribution-ShareAlike 4.0 International License |#
#|    Harold Abelson and Gerald Jay Sussman with Julie Sussman         |#
#|    Modified by TRICK Winners and Judges                             |#

; using Scheme

(DEFINE SET_CAR! . (SET-CAR!))
(DEFINE SET_CDR! . (SET-CDR!))
(DEFINE EOF_OBJECT? . (EOF-OBJECT?))
(DEFINE SYM_BEGIN . ([QUOTE . (
                      #|
                      :\
                      # |#
                      BEGIN)]))

(DEFINE (METACIRCULAR_EVAL EXP . (EV)) . (
  (COND ((SELF_EVALUATING? EXP) . (EXP)) . (
        ((VARIABLE? EXP) . ((LOOKUP_VARIABLE_VALUE EXP . (EV)))) . (
        ((QUOTED? EXP) . ((TEXT_OF_QUOTATION EXP))) . (
        ((ASSIGNMENT? EXP) . ((EVAL_ASSIGNMENT EXP . (EV)))) . (
        ((DEFINITION? EXP) . ((EVAL_DEFINITION EXP . (EV)))) . (
        ((IF? EXP) . ((EVAL_IF EXP . (EV)))) . (
        ((LAMBDA? EXP) . (
         (MAKE_PROCEDURE (LAMBDA_PARAMETERS EXP) . (
                         (LAMBDA_BODY EXP) .
                         (EV))))) . (
        ((BEGIN? EXP) . (
         (EVAL_SEQUENCE (BEGIN_ACTIONS EXP) . (EV)))) . (
        ((COND? EXP) . ((METACIRCULAR_EVAL (COND2IF EXP) . (EV)))) . (
        ((APPLICATION? EXP) . (
         (METACIRCULAR_APPLY (METACIRCULAR_EVAL (OPERATOR EXP) . (EV)) . (
                (LIST_OF_VALUES (OPERANDS EXP) . (EV)))))) . (
        [ELSE . (
         (ERROR "UNKNOWN EXPRESSION TYPE -- EVAL" . (EXP)))])))))))))))))

(DEFINE (METACIRCULAR_APPLY PROCEDURE . (ARGUMENTS)) . (
  (COND ((PRIMITIVE_PROCEDURE? PROCEDURE) . (
         (APPLY_PRIMITIVE_PROCEDURE PROCEDURE . (ARGUMENTS)))) . (
        ((COMPOUND_PROCEDURE? PROCEDURE) . (
          [EVAL_SEQUENCE . (
           (PROCEDURE_BODY PROCEDURE) . (
           [EXTEND_ENVIRONMENT . (
            (PROCEDURE_PARAMETERS PROCEDURE) . (
            ARGUMENTS . (
            (PROCEDURE_ENVIRONMENT PROCEDURE))))]))])) . (
        [ELSE . (
          [ERROR . (
           "UNKNOWN PROCEDURE TYPE -- APPLY" . (PROCEDURE))])])))))

(DEFINE (LIST_OF_VALUES EXPS . (EV)) . (
  (IF (NO_OPERANDS? EXPS) . (
      (QUOTE . (())) . (
      (CONS (METACIRCULAR_EVAL (FIRST_OPERAND EXPS) . (EV)) . (
             (LIST_OF_VALUES (REST_OPERANDS EXPS) . (EV)))))))))

(DEFINE (EVAL_IF EXP . (EV)) . (
  (IF (TRUE? (METACIRCULAR_EVAL (IF_PREDICATE EXP) . (EV))) . (
      (METACIRCULAR_EVAL (IF_CONSEQUENT EXP) . (EV)) . (
      (METACIRCULAR_EVAL (IF_ALTERNATIVE EXP) . (EV)))))))

(DEFINE (EVAL_SEQUENCE EXPS . (EV)) . (
  (COND ((LAST_EXP? EXPS) . ((METACIRCULAR_EVAL (FIRST_EXP EXPS) . (EV)))) . (
        [ELSE . (
         (METACIRCULAR_EVAL (FIRST_EXP EXPS) . (EV)) . (
         (EVAL_SEQUENCE (REST_EXPS EXPS) . (EV))))]))))

(DEFINE (EVAL_ASSIGNMENT EXP . (EV)) . (
  (SET_VARIABLE_VALUE! (ASSIGNMENT_VARIABLE EXP) . (
                       (METACIRCULAR_EVAL (ASSIGNMENT_VALUE EXP) . (EV)) . (
                       EV))) . (
  (QUOTE OK))))

(DEFINE (EVAL_DEFINITION EXP . (EV)) . (
  (DEFINE_VARIABLE! (DEFINITION_VARIABLE EXP) . (
                    (METACIRCULAR_EVAL (DEFINITION_VALUE EXP) . (EV)) . (
                    EV))) . (
  (QUOTE OK))))

(DEFINE (SELF_EVALUATING? EXP) . (
  (COND ((NUMBER? EXP) . ([NUMBER? . (0)])) . (
        ((STRING? EXP) . ([NUMBER? . (0)])) . (
        [ELSE . ([NUMBER? . ("")])])))))

(DEFINE (VARIABLE? EXP) . ([SYMBOL? . (EXP)]))

(DEFINE (QUOTED? EXP) . (
  (TAGGED_LIST? EXP . ((QUOTE QUOTE)))))

(DEFINE (TEXT_OF_QUOTATION EXP) . ((CAR (CDR EXP))))

(DEFINE (TAGGED_LIST? . (EXP . (TAG))) . (
  (IF (PAIR? EXP) . (
      (EQ? (CAR EXP) . (TAG)) . (
      (NUMBER? ""))))))

(DEFINE (ASSIGNMENT? EXP) . (
  (TAGGED_LIST? EXP . ((QUOTE SET!)))))
(DEFINE (ASSIGNMENT_VARIABLE EXP) . ((CAR (CDR EXP))))
(DEFINE (ASSIGNMENT_VALUE EXP) . ((CAR (CDR (CDR EXP)))))

(DEFINE (DEFINITION? EXP) . (
  (TAGGED_LIST? EXP . ((QUOTE DEFINE)))))
(DEFINE (DEFINITION_VARIABLE EXP) . (
  (IF (SYMBOL? (CAR (CDR EXP))) . (
      (CAR (CDR EXP)) . (
      (CAR (CAR (CDR EXP))))))))
(DEFINE (DEFINITION_VALUE EXP) . (
  (IF (SYMBOL? (CAR (CDR EXP))) . (
      (CAR (CDR (CDR EXP))) . (
      (MAKE_LAMBDA (CDR (CAR (CDR EXP))) . (
                   (CDR (CDR EXP)))))))))

(DEFINE (LAMBDA? EXP) . ((TAGGED_LIST? EXP . ((QUOTE LAMBDA)))))
(DEFINE (LAMBDA_PARAMETERS EXP) . ((CAR (CDR EXP))))
(DEFINE (LAMBDA_BODY EXP) . ((CDR (CDR EXP))))

(DEFINE (MAKE_LAMBDA . (PARAMETERS . (BODY))) . (
  (CONS (QUOTE LAMBDA) . ((CONS PARAMETERS . (BODY))))))

(DEFINE (IF? EXP) . ((TAGGED_LIST? EXP . ((QUOTE IF)))))
(DEFINE (IF_PREDICATE EXP) . ((CAR (CDR EXP))))
(DEFINE (IF_CONSEQUENT EXP) . ((CAR (CDR (CDR EXP)))))
(DEFINE (IF_ALTERNATIVE EXP) . (
  (IF (NOT (NULL? (CDR (CDR (CDR EXP))))) . (
      (CAR (CDR (CDR (CDR EXP)))) . (
      (QUOTE FALSE))))))

(DEFINE (MAKE_IF . (PREDICATE . (CONSEQUENT . (ALTERNATIVE)))) . (
  (LIST (QUOTE IF) . (PREDICATE . (CONSEQUENT . (ALTERNATIVE))))))

(DEFINE (BEGIN? EXP) . ((TAGGED_LIST? EXP . (SYM_BEGIN))))
(DEFINE (BEGIN_ACTIONS EXP) . ((CDR EXP)))
(DEFINE (LAST_EXP? SEQ) . ((NULL? (CDR SEQ))))
(DEFINE (FIRST_EXP SEQ) . ((CAR SEQ)))
(DEFINE (REST_EXPS SEQ) . ((CDR SEQ)))

(DEFINE (SEQUENCE2EXP SEQ) . (
  (COND ((NULL? SEQ) . (SEQ)) . (
        ((LAST_EXP? SEQ) . ((FIRST_EXP SEQ))) . (
        [ELSE . ((MAKE_BEGIN SEQ))])))))
(DEFINE (MAKE_BEGIN SEQ) . ((CONS SYM_BEGIN . (SEQ))))

(DEFINE (APPLICATION? EXP) . ((PAIR? EXP)))
(DEFINE (OPERATOR EXP) . ((CAR EXP)))
(DEFINE (OPERANDS EXP) . ((CDR EXP)))
(DEFINE (NO_OPERANDS? OPS) . ((NULL? OPS)))
(DEFINE (FIRST_OPERAND OPS) . ((CAR OPS)))
(DEFINE (REST_OPERANDS OPS) . ((CDR OPS)))

(DEFINE (COND? EXP) . ((TAGGED_LIST? EXP . ((QUOTE COND)))))
(DEFINE (COND_CLAUSES EXP) . ((CDR EXP)))
(DEFINE (COND_ELSE_CLAUSE? CLAUSE) . (
  (EQ? (COND_PREDICATE CLAUSE) . ((QUOTE ELSE)))))
(DEFINE (COND_PREDICATE CLAUSE) . ((CAR CLAUSE)))
(DEFINE (COND_ACTIONS CLAUSE) . ((CDR CLAUSE)))
(DEFINE (COND2IF EXP) . (
  (EXPAND_CLAUSES (COND_CLAUSES EXP))))

(DEFINE (EXPAND_CLAUSES CLAUSES) . (
  (IF (NULL? CLAUSES) . (
      (QUOTE FALSE) . (
      (LET ((FIRST (CAR CLAUSES)) . (
            (REST (CDR CLAUSES)))) . (
        (IF (COND_ELSE_CLAUSE? FIRST) . (
            (IF (NULL? REST) . (
                (SEQUENCE2EXP (COND_ACTIONS FIRST)) . (
                (ERROR "ELSE CLAUSE ISN'T LAST -- COND2IF" . (
                       CLAUSES))))) . (
            (MAKE_IF (COND_PREDICATE FIRST) . (
                     (SEQUENCE2EXP (COND_ACTIONS FIRST)) . (
                     (EXPAND_CLAUSES REST))))))))))))))

(DEFINE (TRUE? X) . (
  (NOT (EQ? X . ((NUMBER? ""))))))
(DEFINE (FALSE? X) . (
  (EQ? X . ((NUMBER? "")))))

(DEFINE (MAKE_PROCEDURE PARAMETERS . (BODY . (EV))) . (
  (LIST (QUOTE PROCEDURE) . (PARAMETERS . (BODY . (EV))))))
(DEFINE (COMPOUND_PROCEDURE? P) . (
  (TAGGED_LIST? P . ((QUOTE PROCEDURE)))))
(DEFINE (PROCEDURE_PARAMETERS P) . ((CAR (CDR P))))
(DEFINE (PROCEDURE_BODY P) . ((CAR (CDR (CDR P)))))
(DEFINE (PROCEDURE_ENVIRONMENT P) . ((CAR (CDR (CDR (CDR P))))))

(DEFINE (ENCLOSING_ENVIRONMENT EV) . ((CDR EV)))
(DEFINE (FIRST_FRAME EV) . ((CAR EV)))
(DEFINE THE_EMPTY_ENVIRONMENT . ([QUOTE . (())]))

(DEFINE (MAKE_FRAME VARIABLES . (VALUES)) . (
  (CONS VARIABLES . (VALUES))))
(DEFINE (FRAME_VARIABLES FRAME) . ((CAR FRAME)))
(DEFINE (FRAME_VALUES FRAME) . ((CDR FRAME)))
(DEFINE (ADD_BINDING_TO_FRAME! VAR . (VAL . (FRAME))) . (
  (SET_CAR! FRAME . ((CONS VAR . ((CAR FRAME))))) . (
  (SET_CDR! FRAME . ((CONS VAL . ((CDR FRAME))))))))

(DEFINE (EXTEND_ENVIRONMENT VARS . (VALS . (BASE_ENV))) . (
  (IF (EQ? (LENGTH VARS) . ((LENGTH VALS))) . (
      (CONS (MAKE_FRAME VARS . (VALS)) . (BASE_ENV)) . (
      (IF (EQ? (LENGTH VARS) . ((MIN (LENGTH VARS) . ((LENGTH VALS))))) . (
          (ERROR "TOO MANY ARGUMENTS SUPPLIED" . (VARS . (VALS))) . (
          (ERROR "TOO FEW ARGUMENTS SUPPLIED" . (VARS . (VALS)))))))))))

(DEFINE (LOOKUP_VARIABLE_VALUE VAR . (EV)) . (
  (DEFINE (ENV_LOOP EV) . (
    (DEFINE (SCAN VARS . (VALS)) . (
      (COND ((NULL? VARS) . (
             (ENV_LOOP (ENCLOSING_ENVIRONMENT EV)))) . (
            ((EQ? VAR . ((CAR VARS))) . (
              (CAR VALS))) . (
            [ELSE . ((SCAN (CDR VARS) . ((CDR VALS))))]))))) . (
    (IF (EQ? EV . (THE_EMPTY_ENVIRONMENT)) . (
        (ERROR "UNBOUND VARIABLE" . (VAR)) . (
        (LET [(FRAME (FIRST_FRAME EV))] . (
          (SCAN (FRAME_VARIABLES FRAME) . (
                (FRAME_VALUES FRAME))))))))))) . (
  (ENV_LOOP EV))))

(DEFINE (SET_VARIABLE_VALUE! VAR . (VAL . (EV))) . (
  (DEFINE (ENV_LOOP EV) . (
    (DEFINE (SCAN VARS . (VALS)) . (
      (COND ((NULL? VARS) . (
             (ENV_LOOP (ENCLOSING_ENVIRONMENT EV)))) . (
            ((EQ? VAR . ((CAR VARS))) . (
             (SET_CAR! VALS . (VAL)))) . (
            [ELSE . ((SCAN (CDR VARS) . ((CDR VALS))))]))))) . (
    (IF (EQ? EV . (THE_EMPTY_ENVIRONMENT)) . (
        (ERROR "UNBOUND VARIABLE -- SET!" . (VAR)) . (
        (LET [(FRAME (FIRST_FRAME EV))] . (
          (SCAN (FRAME_VARIABLES FRAME) . (
                (FRAME_VALUES FRAME))))))))))) . (
  (ENV_LOOP EV))))

(DEFINE (DEFINE_VARIABLE! VAR . (VAL . (EV))) . (
  (LET [(FRAME (FIRST_FRAME EV))] . (
    (DEFINE (SCAN VARS . (VALS)) . (
      (COND ((NULL? VARS) . (
             (ADD_BINDING_TO_FRAME! VAR . (VAL . (FRAME))))) . (
            ((EQ? VAR . ((CAR VARS))) . (
             (SET_CAR! VALS . (VAL)))) . (
            [ELSE . ((SCAN (CDR VARS) . ((CDR VALS))))]))))) . (
    (SCAN (FRAME_VARIABLES FRAME) . (
          (FRAME_VALUES FRAME))))))))

(DEFINE (SETUP_ENVIRONMENT . ()) . (
  (LET ([INITIAL_ENV . (
         (EXTEND_ENVIRONMENT (PRIMITIVE_PROCEDURE_NAMES . ()) . (
                             (PRIMITIVE_PROCEDURE_OBJECTS . ()) . (
                             THE_EMPTY_ENVIRONMENT))))]) . (
    (DEFINE_VARIABLE! (QUOTE TRUE) . ((NUMBER? . (0)) . (INITIAL_ENV))) . (
    (DEFINE_VARIABLE! (QUOTE FALSE) . ((NUMBER? . ("")) . (INITIAL_ENV))) . (
    INITIAL_ENV))))))

(DEFINE (PRIMITIVE_PROCEDURE? PROC) . (
  (TAGGED_LIST? PROC . ((QUOTE PRIMITIVE)))))

(DEFINE (PRIMITIVE_IMPLEMENTATION PROC) . ((CAR (CDR PROC))))

(DEFINE PRIMITIVE_PROCEDURES . (
  (LIST (LIST (QUOTE CAR) . (CAR)) . (
        (LIST (QUOTE CDR) . (CDR)) . (
        (LIST (QUOTE CONS) . (CONS)) . (
        (LIST (QUOTE NULL?) . (NULL?)) . (
        (LIST (QUOTE EQ?) . (EQ?)) . (
        (LIST (QUOTE DISPLAY) . (DISPLAY)) . (
        (LIST (CAR (CDR (QUOTE (A * A)))) . ((CAR (LIST * LIST)))) . (
        (LIST (CAR (QUOTE (- A))) . ((CAR (LIST - 0))))))))))))
        ))
(DEFINE (PRIMITIVE_PROCEDURE_NAMES . ()) . (
  (MAP CAR . (
       PRIMITIVE_PROCEDURES))))

(DEFINE (PRIMITIVE_PROCEDURE_OBJECTS . ()) . (
  (MAP (LAMBDA (PROC . ()) . ([LIST . ((QUOTE PRIMITIVE) . ((CAR (CDR PROC))))])) . (
       PRIMITIVE_PROCEDURES))))

(DEFINE (APPLY_PRIMITIVE_PROCEDURE PROC . (ARGS)) . (
  (APPLY (PRIMITIVE_IMPLEMENTATION PROC) . (ARGS))))

(DEFINE INPUT_PROMPT . (";;; M_EVAL INPUT:"))
(DEFINE OUTPUT_PROMPT . (";;; M_EVAL VALUE:"))
(DEFINE (DRIVER_LOOP . ()) . (
  (PROMPT_FOR_INPUT INPUT_PROMPT) . (
  (LET [(INPUT (READ . ()))] . (
    (IF (NOT (EOF_OBJECT? INPUT)) . (
        (LET [(OUTPUT (METACIRCULAR_EVAL INPUT . (THE_GLOBAL_ENVIRONMENT)))] . (
          (ANNOUNCE_OUTPUT OUTPUT_PROMPT) . (
          (USER_PRINT OUTPUT) . (
          (DRIVER_LOOP . ()))))))))))))

(DEFINE (PROMPT_FOR_INPUT STRING) . (
  (DISPLAY "\n") . ((DISPLAY "\n") . ((DISPLAY STRING) . ((DISPLAY "\n"))))))

(DEFINE (ANNOUNCE_OUTPUT STRING) . (
  (DISPLAY "\n") . ((DISPLAY STRING) . ((DISPLAY "\n")))))

(DEFINE (USER_PRINT OBJECT) . (
  (IF (COMPOUND_PROCEDURE? OBJECT) . (
      (DISPLAY (LIST (QUOTE COMPOUND_PROCEDURE) . (
                     (PROCEDURE_PARAMETERS OBJECT) . (
                     (PROCEDURE_BODY OBJECT) . (
                     (QUOTE PROCEDURE_ENV)))))) . (
      (DISPLAY OBJECT))))))

(DEFINE THE_GLOBAL_ENVIRONMENT . ((SETUP_ENVIRONMENT . ())))

(DRIVER_LOOP . ())
