*&---------------------------------------------------------------------*
*& Report ZEOS_PROVISION_REPORT
*& Transaction: ZEOSPRROREP
*& Description: End Of Service Provision Report
*& Feedback: With zero status - showing active employees
*& Status: Tr shared
*&---------------------------------------------------------------------*
REPORT zeos_provision_report.

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: pa0001,  " HR Master Record: Infotype 0001 (Org. Assignment)
        pa0000.  " HR Master Record: Infotype 0000 (Actions)

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_employee,
         pernr TYPE pa0001-pernr,      " Personnel Number
         ename TYPE pa0001-ename,      " Employee Name
         bukrs TYPE pa0001-bukrs,      " Company Code
         werks TYPE pa0001-werks,      " Personnel Area
         btrtl TYPE pa0001-btrtl,      " Personnel Subarea
         persg TYPE pa0001-persg,      " Employee Group
         persk TYPE pa0001-persk,      " Employee Subgroup
         orgeh TYPE pa0001-orgeh,      " Organizational Unit
         plans TYPE pa0001-plans,      " Position
         stell TYPE pa0001-stell,      " Job
         begda TYPE pa0001-begda,      " Start Date
         endda TYPE pa0001-endda,      " End Date
         stat2 TYPE pa0000-stat2,      " Employment Status
         provision_amount TYPE p DECIMALS 2,  " EOS Provision Amount
       END OF ty_employee.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_employees TYPE TABLE OF ty_employee,
      gs_employee  TYPE ty_employee,
      gv_total     TYPE p DECIMALS 2.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_pernr FOR pa0001-pernr,      " Personnel Number
                  s_bukrs FOR pa0001-bukrs,      " Company Code
                  s_werks FOR pa0001-werks,      " Personnel Area
                  s_orgeh FOR pa0001-orgeh.      " Org Unit
  PARAMETERS:     p_date  TYPE sy-datum DEFAULT sy-datum.  " Key Date
  PARAMETERS:     p_zero  TYPE c AS CHECKBOX DEFAULT 'X'.  " Include Zero
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_active TYPE c AS CHECKBOX DEFAULT 'X'.  " Active Only
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  TEXT-001 = 'Selection Criteria'.
  TEXT-002 = 'Employee Status'.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_employees.
  PERFORM calculate_provisions.
  PERFORM display_report.

*&---------------------------------------------------------------------*
*& Form SELECT_EMPLOYEES
*&---------------------------------------------------------------------*
FORM select_employees.

  SELECT a~pernr a~ename a~bukrs a~werks a~btrtl
         a~persg a~persk a~orgeh a~plans a~stell
         a~begda a~endda b~stat2
    INTO CORRESPONDING FIELDS OF TABLE gt_employees
    FROM pa0001 AS a
    INNER JOIN pa0000 AS b ON a~pernr = b~pernr
    WHERE a~pernr IN s_pernr
      AND a~bukrs IN s_bukrs
      AND a~werks IN s_werks
      AND a~orgeh IN s_orgeh
      AND p_date BETWEEN a~begda AND a~endda
      AND p_date BETWEEN b~begda AND b~endda.

* Filter by active status if checkbox selected
  IF p_active = 'X'.
    DELETE gt_employees WHERE stat2 <> '3'.  " Active status
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALCULATE_PROVISIONS
*&---------------------------------------------------------------------*
FORM calculate_provisions.

  DATA: lv_years TYPE i,
        lv_provision TYPE p DECIMALS 2.

  LOOP AT gt_employees INTO gs_employee.
*   Calculate years of service
    lv_years = ( p_date - gs_employee-begda ) / 365.

*   Calculate EOS provision (simplified calculation)
*   Actual calculation should be based on labor law
    lv_provision = lv_years * 1000.  " Placeholder calculation

    gs_employee-provision_amount = lv_provision.
    MODIFY gt_employees FROM gs_employee.

    gv_total = gv_total + lv_provision.
  ENDLOOP.

* Remove zero provisions if not requested
  IF p_zero <> 'X'.
    DELETE gt_employees WHERE provision_amount = 0.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_REPORT
*&---------------------------------------------------------------------*
FORM display_report.

  DATA: lo_alv TYPE REF TO cl_salv_table.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_employees ).

      lo_alv->display( ).

    CATCH cx_salv_msg.
*     Fallback to simple write
      PERFORM display_simple.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_SIMPLE
*&---------------------------------------------------------------------*
FORM display_simple.

  WRITE: / 'End Of Service Provision Report'.
  WRITE: / 'Date:', p_date.
  ULINE.

  WRITE: / 'Pernr', 15 'Name', 40 'Company', 50 'Provision'.
  ULINE.

  LOOP AT gt_employees INTO gs_employee.
    WRITE: / gs_employee-pernr,
             15 gs_employee-ename,
             40 gs_employee-bukrs,
             50 gs_employee-provision_amount.
  ENDLOOP.

  ULINE.
  WRITE: / 'Total Provision:', 50 gv_total.

ENDFORM.
