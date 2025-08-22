CLASS zcl_itab_basics DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .


  PUBLIC SECTION.
    TYPES group TYPE c LENGTH 1.
    TYPES: BEGIN OF initial_type,
             group       TYPE group,
             number      TYPE i,
             description TYPE string,
           END OF initial_type,
           itab_data_type TYPE STANDARD TABLE OF initial_type WITH EMPTY KEY.

    METHODS fill_itab
           RETURNING
             VALUE(initial_data) TYPE itab_data_type.

    METHODS add_to_itab
           IMPORTING initial_data TYPE itab_data_type
           RETURNING
            VALUE(updated_data) TYPE itab_data_type.

    METHODS sort_itab
           IMPORTING initial_data TYPE itab_data_type
           RETURNING
            VALUE(updated_data) TYPE itab_data_type.

    METHODS search_itab
           IMPORTING initial_data TYPE itab_data_type
           RETURNING
             VALUE(result_index) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_itab_basics IMPLEMENTATION.
  METHOD fill_itab.
    initial_data = VALUE itab_data_type( ( GROUP = 'A' NUMBER = 10 DESCRIPTION = 'Group A-2')
                                         ( GROUP = 'B' NUMBER = 5 DESCRIPTION = 'Group B')
                                         ( GROUP = 'A' NUMBER = 6 DESCRIPTION = 'Group A-1')
                                         ( GROUP = 'C' NUMBER = 22 DESCRIPTION = 'Group C-1')
                                         ( GROUP = 'A' NUMBER = 13 DESCRIPTION = 'Group A-3')
                                         ( GROUP = 'C' NUMBER = 500 DESCRIPTION = 'Group C-2')      ).
  ENDMETHOD.

  METHOD add_to_itab.
    updated_data = initial_data.
    APPEND VALUE #( GROUP = 'A' NUMBER = 19 DESCRIPTION = 'Group A-4') TO updated_data.
  ENDMETHOD.

  METHOD sort_itab.
    updated_data = initial_data.
    SORT updated_data BY group ASCENDING number DESCENDING.
  ENDMETHOD.

  METHOD search_itab.
    DATA(temp_data) = initial_data.
    result_index = LINE_INDEX( temp_data[ number = 6 ] ).
  ENDMETHOD.

ENDCLASS.
