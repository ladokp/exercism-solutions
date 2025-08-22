CLASS zcl_leap DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS leap
      IMPORTING
        year          TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS zcl_leap IMPLEMENTATION.

  METHOD leap.
    IF year MOD 4 <> 0 OR year MOD 100 = 0 AND year MOD 400 <> 0.
      result = abap_false.
    ELSE.
      result = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
