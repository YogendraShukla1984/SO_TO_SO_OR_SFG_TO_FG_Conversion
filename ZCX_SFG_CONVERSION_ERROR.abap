*&---------------------------------------------------------------------*
*& Class ZCX_SFG_CONVERSION_ERROR
*&---------------------------------------------------------------------*
*& Purpose: Exception class for SFG to FG conversion errors
*& Author: System
*& Creation Date: 2025-12-31
*&---------------------------------------------------------------------*

" BEGIN: Cursor Generated Code

CLASS zcx_sfg_conversion_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.
    INTERFACES if_t100_dyn_msg.

    CONSTANTS:
      BEGIN OF date_range_invalid,
        msgid TYPE symsgid VALUE 'ZSCM_SFG',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF date_range_invalid,

      BEGIN OF no_data_found,
        msgid TYPE symsgid VALUE 'ZSCM_SFG',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_data_found,

      BEGIN OF activity_type_invalid,
        msgid TYPE symsgid VALUE 'ZSCM_SFG',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MV_ACTIVITY_TYPE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF activity_type_invalid,

      BEGIN OF ecc_connection_error,
        msgid TYPE symsgid VALUE 'ZSCM_SFG',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_ERROR_TEXT',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF ecc_connection_error,

      BEGIN OF authorization_failed,
        msgid TYPE symsgid VALUE 'ZSCM_SFG',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_PLANT',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF authorization_failed,

      BEGIN OF unexpected_error,
        msgid TYPE symsgid VALUE 'ZSCM_SFG',
        msgno TYPE symsgno VALUE '999',
        attr1 TYPE scx_attrname VALUE 'MV_ERROR_TEXT',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF unexpected_error.

    " Public attributes for message parameters
    DATA: mv_error_text    TYPE string,
          mv_activity_type TYPE char20,
          mv_plant         TYPE werks_d.

    " Constructor
    METHODS constructor
      IMPORTING
        textid           LIKE if_t100_message=>t100key OPTIONAL
        previous         LIKE previous OPTIONAL
        mv_error_text    TYPE string OPTIONAL
        mv_activity_type TYPE char20 OPTIONAL
        mv_plant         TYPE werks_d OPTIONAL.
ENDCLASS.

CLASS zcx_sfg_conversion_error IMPLEMENTATION.
  METHOD constructor.
    " Call superclass constructor
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    " Set attributes
    me->mv_error_text = mv_error_text.
    me->mv_activity_type = mv_activity_type.
    me->mv_plant = mv_plant.

    " Set message key
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

" END: Cursor Generated Code


