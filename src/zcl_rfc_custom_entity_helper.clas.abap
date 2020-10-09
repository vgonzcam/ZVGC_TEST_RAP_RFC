"#autoformat
"! <p class="shorttext synchronized" lang="en">Helper class.</p>
"! <p> Given a <strong>SAP Function module</strong> returns an
"! <em>JSON</em> with the FM parameters as a DDL source code for custom entities
"! of the passed FM.</p>
CLASS zcl_rfc_custom_entity_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_params,
             fm_name TYPE eu_lname,
             name    TYPE char40,
             params  TYPE rsfb_para,
           END OF ty_params.
    TYPES   ty_t_params TYPE STANDARD TABLE OF ty_params.

    TYPES: BEGIN OF ty_declaration,
             name   TYPE string,
             declar TYPE cim_t_string,
           END OF ty_declaration.
    TYPES   ty_t_declaration    TYPE STANDARD TABLE OF ty_declaration.

    INTERFACES if_oo_adt_classrun.


    "! <p class="shorttext synchronized" lang="en">Full process</p>
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en">FM name</p>
    "! @parameter show_info | <p class="shorttext synchronized" lang="en">Internal SAP use. CL_DEMO_OUTPUT</p>
    "! @parameter result | <p class="shorttext synchronized" lang="en">Result as a JSON</p>
    "! @parameter ddl_info | <p class="shorttext synchronized" lang="en">Result ITAB format</p>
    METHODS get_ddl
      IMPORTING
        fm_name         TYPE string
        show_info       TYPE abap_bool DEFAULT abap_false
      EXPORTING
        VALUE(result)   TYPE string
        VALUE(ddl_info) TYPE ty_t_declaration.

  PROTECTED SECTION.

  PRIVATE SECTION.

    "! <p class="shorttext synchronized" lang="en">Helper class.</p>
    "! <p> Given a <strong>SAP Function module</strong> returns an
    "! <em>JSON</em> with the FM parameters of the passed FM.</p>
    METHODS class_docu.

    "! <p class="shorttext synchronized" lang="en">Get FM params</p>
    "!
    "! @parameter fm_name | <p class="shorttext synchronized" lang="en">Function Module NAME</p>
    "! @parameter result | <p class="shorttext synchronized" lang="en">Parameters detail as a JSON</p>
    "! @parameter params | <p class="shorttext synchronized" lang="en">Parameters as a IT</p>
    "! References: https://wiki.scn.sap.com/wiki/display/ABAP/Extracting+Function+Module+Details+using+Class+-+CL_FB_FUNCTION_UTILITY
    METHODS get_info
      IMPORTING
        fm_name       TYPE string
      EXPORTING
        VALUE(result) TYPE string
        VALUE(params) TYPE ty_t_params.

    "! <p class="shorttext synchronized" lang="en">Get DDL from a stricture reference</p>
    "!
    "! @parameter struct_name | <p class="shorttext synchronized" lang="en">Structure Type</p>
    "! @parameter ddl_declar | <p class="shorttext synchronized" lang="en">Types information</p>
    "! Reference: https://blogs.sap.com/2019/03/01/how-to-generate-the-ddl-source-code-for-custom-entities-that-are-implemented-by-remote-rfc-calls/
    METHODS get_struct_info
      IMPORTING
        struct_name       TYPE rs38l_typ
      EXPORTING
        VALUE(ddl_declar) TYPE ty_t_declaration.

ENDCLASS.



CLASS zcl_rfc_custom_entity_helper IMPLEMENTATION.

  METHOD class_docu.
  ENDMETHOD.

  METHOD get_info.
    TRY.
        DATA: lv_fm_name TYPE eu_lname.
        lv_fm_name = fm_name.

* Header des FB lesen
        DATA: lv_head       TYPE header_fb.
        DATA: lv_interface  TYPE rsfbintfv.

        cl_fb_function_utility=>meth_get_header_fb( EXPORTING im_name   = lv_fm_name
                                                    IMPORTING ex_header = lv_head ).

        cl_fb_function_utility=>meth_get_interface( EXPORTING im_name      = lv_fm_name
                                                    IMPORTING ex_interface = lv_interface ).

        DATA: lt_params TYPE ty_t_params.
        lt_params  = VALUE #( ( fm_name =  lv_fm_name name = 'IMPORT' params = lv_interface-import )
                              ( fm_name =  lv_fm_name name = 'EXPORT' params = lv_interface-export )
                              ( fm_name =  lv_fm_name name = 'CHANGE' params = lv_interface-change )
                              ( fm_name =  lv_fm_name name = 'TABLES' params = lv_interface-tables ) ).

        DATA(gv_json_output) = /ui2/cl_json=>serialize( data = lt_params  compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

        result = gv_json_output.
        params = lt_params.

      CATCH cx_root INTO DATA(e_txt).
        WRITE: / e_txt->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_struct_info.

    "***************************************************************************************
    "* ABAP DDIC types
    "* https://help.sap.com/doc/abapdocu_751_index_htm/7.51/en-US/abenddic_builtin_types.htm
    "***************************************************************************************
    "* ABAP CDS types
    "* https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abencds_typing.htm
    "***************************************************************************************
    DATA:
      lo_struct_desc          TYPE REF TO cl_abap_structdescr,
      lo_type_desc            TYPE REF TO cl_abap_typedescr,
      lo_elem_desc            TYPE REF TO cl_abap_elemdescr,
      lt_comp_table           TYPE cl_abap_structdescr=>component_table,
      lv_length               TYPE i,
      lv_p_length             TYPE i,
      lv_decimals             TYPE i,
      lv_abap_cds_data_type   TYPE abap_typename,
      lv_abap_ddic_data_type  TYPE string,
      lv_property             TYPE string,
      ls_ddic_fields          TYPE dfies,
      lv_ddl_source_code_line TYPE string,
      lt_ddl_source_code      TYPE STANDARD TABLE OF string,
      lv_reffield_abap        TYPE dd03l-reffield,
      lv_refproperty          TYPE string.

    DATA: lv_rfc_input_structure TYPE string.
    lv_rfc_input_structure = struct_name.

    "name of structure must be in uppercase because otherwise reference fields for currency and unit
    "are not found in DD03L
    lv_rfc_input_structure = to_upper( lv_rfc_input_structure ).

    lo_type_desc =  cl_abap_typedescr=>describe_by_name( lv_rfc_input_structure ).

    "check that the object is a structure
    TRY.
        IF lo_type_desc->kind = lo_type_desc->kind_struct.
          lo_struct_desc ?= lo_type_desc.
          "this method retrieves the components also from structures with append structures
          /iwbep/cl_mgw_med_model_util=>get_structure_components(
            EXPORTING
              io_structure_descriptor = lo_struct_desc
            CHANGING
              ct_components = lt_comp_table ).
        ELSE.
          "@todo: raise a more meaningful exception here
          RAISE EXCEPTION TYPE /iwbep/cx_mgw_med_exception
            EXPORTING
              textid = /iwbep/cx_mgw_med_exception=>otr_text_read_error.
        ENDIF.

      CATCH /iwbep/cx_mgw_med_exception.
        "handle exception
        EXIT.
    ENDTRY.

    "add a comment that contains the name of the structure being used as input
    lv_ddl_source_code_line = |// \{ lv_rfc_input_structure \} Start DDL source code for custom entity for { lv_rfc_input_structure }|.
    APPEND lv_ddl_source_code_line TO lt_ddl_source_code.
    "add a comment that contains information when and where the code has been generated
    lv_ddl_source_code_line = |// generated on: { sy-datum } at: { sy-uzeit } in: { sy-sysid } |.
    APPEND lv_ddl_source_code_line TO lt_ddl_source_code.

    LOOP AT lt_comp_table INTO DATA(ls_comp_table)  .

      "only elements
      IF ls_comp_table-type->kind <> cl_abap_typedescr=>kind_elem.
        CONTINUE.
      ENDIF.

      lo_elem_desc  ?= ls_comp_table-type.

      lo_elem_desc->get_ddic_field(
          EXPORTING
              p_langu      = sy-langu
          RECEIVING
              p_flddescr   = ls_ddic_fields
          EXCEPTIONS
              not_found    = 1
              no_ddic_type = 2
              OTHERS       = 3
              ).


      lv_property = to_mixed( val = ls_comp_table-name sep = '_' ).
      lv_length = ls_ddic_fields-leng.
      lv_p_length = lv_length DIV 2 + 1.
      lv_decimals = ls_ddic_fields-decimals.

      "add the ABAP field name of the structure as a comment
      "lv_ddl_source_code_line = |// { ls_comp_table-name } |.
      "APPEND lv_ddl_source_code_line TO lt_ddl_source_code.

      "first check for the domain name
      CASE ls_ddic_fields-domname.

        WHEN 'TZNTSTMPL' OR 'TZNTSTMPSL' OR 'TZNTSTMPS' OR 'TIMESTAMP_CHAR'.
          lv_ddl_source_code_line  = |{ lv_property } : timestampl; | .
        WHEN 'TZNTIMELOC'.
          lv_abap_cds_data_type = 'tims'.
          lv_abap_ddic_data_type = ls_ddic_fields-domname.
          lv_ddl_source_code_line = |{ lv_property } : abap.{ lv_abap_cds_data_type } ; | .
        WHEN OTHERS.

          "if none of the obvious domain names is found check the data type
          CASE ls_ddic_fields-datatype.

            WHEN 'CHAR'.
              "if char is used sap.displayformat = uppercase is enforced
              IF ls_ddic_fields-lowercase = abap_true.
                lv_abap_cds_data_type = 'sstring'.
                lv_abap_ddic_data_type = 'c'.
              ELSE.
                lv_abap_cds_data_type = 'char'.
                lv_abap_ddic_data_type = 'c'.
              ENDIF.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type }( { lv_length } ) ; | .
            WHEN 'CLNT'.
              lv_abap_cds_data_type = 'clnt'.
              lv_abap_ddic_data_type = 'c'.
              lv_p_length = 3.
              lv_ddl_source_code_line = |//{ lv_property } : abap.{ lv_abap_cds_data_type } ; | .
            WHEN  'CUKY'.
              lv_ddl_source_code_line = '@Semantics.currencyCode: true'.
              APPEND lv_ddl_source_code_line TO lt_ddl_source_code.

              lv_abap_cds_data_type = 'cuky'.
              lv_abap_ddic_data_type = 'c'.
              lv_p_length = 5.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type }( { lv_length } ) ; | .
            WHEN  'CURR'.
              SELECT SINGLE reffield INTO lv_reffield_abap FROM dd03l WHERE tabname = lv_rfc_input_structure AND fieldname = ls_comp_table-name.
              IF lv_reffield_abap IS NOT INITIAL.
                lv_refproperty = to_mixed( val = lv_reffield_abap sep = '_' ).
              ELSE.
                lv_refproperty = '<enter property name>'.
              ENDIF.
              lv_ddl_source_code_line = |@Semantics.amount.currencyCode: '{ lv_refproperty }' |.
              APPEND lv_ddl_source_code_line TO lt_ddl_source_code.

              lv_abap_cds_data_type = 'curr'.
              lv_abap_ddic_data_type = 'p'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type }( { lv_length }, { lv_decimals } ) ; | .
            WHEN  'DATS' .
              lv_abap_cds_data_type = 'dats'.
              lv_abap_ddic_data_type = 'dats'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type } ; | .
            WHEN  'DEC'.
              lv_abap_cds_data_type = 'dec'.
              lv_abap_ddic_data_type = 'p'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type }( { lv_length }, { lv_decimals } ) ; | .
            WHEN  'FLTP'.
              lv_abap_cds_data_type = 'fltp'.
              lv_abap_ddic_data_type = 'f'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type } ; | .
            WHEN  'INT1'.
              lv_abap_cds_data_type = 'int1'.
              lv_abap_ddic_data_type = 'int1'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type } ; | .
            WHEN  'INT2'.
              lv_abap_cds_data_type = 'int2'.
              lv_abap_ddic_data_type = 's'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type } ; | .
            WHEN  'INT4'.
              lv_abap_cds_data_type = 'int4'.
              lv_abap_ddic_data_type = 'i'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type } ; | .
            WHEN  'INT8'.
              lv_abap_cds_data_type = 'int8'.
              lv_abap_ddic_data_type = 'int8'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type } ; | .
            WHEN  'NUMC'.
              lv_abap_cds_data_type = 'numc'.
              lv_abap_ddic_data_type = 'n'.
              lv_ddl_source_code_line = |{ lv_property } : abap.{ lv_abap_cds_data_type }( { lv_length } ) ; | .
            WHEN  'QUAN'.
              SELECT SINGLE reffield INTO lv_reffield_abap FROM dd03l WHERE tabname = lv_rfc_input_structure AND fieldname = ls_comp_table-name.
              IF lv_reffield_abap IS NOT INITIAL.
                lv_refproperty = to_mixed( val = lv_reffield_abap sep = '_' ).
              ELSE.
                lv_refproperty = '<enter property name>'.
              ENDIF.
              lv_ddl_source_code_line = |@Semantics.quantity.unitOfMeasure: '{ lv_refproperty }' |.
              APPEND lv_ddl_source_code_line TO lt_ddl_source_code.
              lv_abap_cds_data_type = 'quan'.
              lv_abap_ddic_data_type = 'p'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type }( { lv_length }, { lv_decimals } ) ; | .
            WHEN  'STRG'.
              lv_abap_cds_data_type = 'string'.
              lv_abap_ddic_data_type = 'string'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type }( { lv_length } ) ; | .
            WHEN  'DATS' .
              lv_abap_cds_data_type = 'dats'.
              lv_abap_ddic_data_type = 'd'.
              lv_ddl_source_code_line = |{ lv_property } : timestampl; | .
            WHEN 'TIMS'.
              lv_abap_cds_data_type = 'tims'.
              lv_abap_ddic_data_type = 't'.
              lv_ddl_source_code_line = |{ lv_property } : abap.{ lv_abap_cds_data_type } ; | .
            WHEN  'UNIT'.
              lv_ddl_source_code_line = '@Semantics.unitOfMeasure: true'.
              APPEND lv_ddl_source_code_line TO lt_ddl_source_code.
              lv_abap_cds_data_type = 'unit'.
              lv_abap_ddic_data_type = 'c'.
              lv_ddl_source_code_line  = |{ lv_property } : abap.{ lv_abap_cds_data_type }( { lv_length } ) ; | ..
            WHEN OTHERS.
              lv_ddl_source_code_line  = |// { ls_comp_table-name } could not be mapped to a standard data element.|.
          ENDCASE.

      ENDCASE.

      APPEND lv_ddl_source_code_line TO lt_ddl_source_code.

    ENDLOOP.

    "add a comment that contains the name of the structure being used as input
    lv_ddl_source_code_line = |// { lv_rfc_input_structure } End of DDL source code for custom entity for { lv_rfc_input_structure }|.
    APPEND lv_ddl_source_code_line TO lt_ddl_source_code.

    INSERT INITIAL LINE INTO TABLE ddl_declar ASSIGNING FIELD-SYMBOL(<ddl>).
    <ddl>-name = lv_rfc_input_structure.
    <ddl>-declar = lt_ddl_source_code.

  ENDMETHOD.

  METHOD get_ddl.
    DATA: lt_params TYPE ty_t_params.
    DATA: lv_return TYPE string.
    DATA: lt_type_declar TYPE ty_t_declaration.

    get_info( EXPORTING fm_name = fm_name IMPORTING params = lt_params  result = lv_return ).

    LOOP AT lt_params ASSIGNING FIELD-SYMBOL(<fs>).
      LOOP AT <fs>-params ASSIGNING FIELD-SYMBOL(<fs_par>).
        get_struct_info( EXPORTING struct_name = <fs_par>-structure IMPORTING ddl_declar = lt_type_declar ).
        IF lt_type_declar IS NOT INITIAL.
          APPEND LINES OF lt_type_declar TO ddl_info.

          CLEAR lt_type_declar.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF ddl_info IS NOT INITIAL.
      DATA(gv_json_output) = /ui2/cl_json=>serialize( data = ddl_info
                                                      compress = abap_true
                                                      pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

      result = gv_json_output.
      IF show_info EQ abap_true.
        cl_demo_output=>display_json( result ).
      ENDIF.
    ELSE.
    ENDIF.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
  ENDMETHOD.

ENDCLASS.
