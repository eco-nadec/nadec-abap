*&---------------------------------------------------------------------*
*& Report ZLEAVE_PROVISION_REPORT
*& Transaction: ZLEAPROREP
*& Description: Leave Provision Report
*& Feedback: With zero status - showing active employees
*& Status: Tr shared
*&---------------------------------------------------------------------*
REPORT zleave_provision_report.

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: pa0001,  " HR Master Record: Infotype 0001 (Org. Assignment)
        pa0000,  " HR Master Record: Infotype 0000 (Actions)
        pa0008.  " HR Master Record: Infotype 0008 (Basic Pay)

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_leave_provision,
         pernr         TYPE pa0001-pernr,      " Personnel Number
         ename         TYPE pa0001-ename,      " Employee Name
         bukrs         TYPE pa0001-bukrs,      " Company Code
         werks         TYPE pa0001-werks,      " Personnel Area
         btrtl         TYPE pa0001-btrtl,      " Personnel Subarea
         kostl         TYPE pa0001-kostl,      " Cost Center
         stat2         TYPE pa0000-stat2,      " Employment Status
         hire_date     TYPE pa0001-begda,      " Hire Date
         basic_salary  TYPE p DECIMALS 2,      " Basic Salary
         leave_balance TYPE p DECIMALS 2,      " Leave Balance (Days)
         provision_amt TYPE p DECIMALS 2,      " Leave Provision Amount
       END OF ty_leave_provision.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_provisions TYPE TABLE OF ty_leave_provision,
      gs_provision  TYPE ty_leave_provision,
      gv_total_prov TYPE p DECIMALS 2.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_pernr FOR pa0001-pernr,      " Personnel Number
                  s_bukrs FOR pa0001-bukrs,      " Company Code
                  s_werks FOR pa0001-werks,      " Personnel Area
                  s_kostl FOR pa0001-kostl.      " Cost Center
  PARAMETERS:     p_date  TYPE sy-datum DEFAULT sy-datum.  " Key Date
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_active TYPE c AS CHECKBOX DEFAULT 'X'.  " Active Employees Only
  PARAMETERS: p_zero   TYPE c AS CHECKBOX DEFAULT ' '.  " Include Zero Provision
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  TEXT-001 = 'Selection Criteria'.
  TEXT-002 = 'Status Filters'.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM select_employees.
  PERFORM get_salary_data.
  PERFORM calculate_provisions.
  PERFORM filter_results.
  PERFORM display_report.

*&---------------------------------------------------------------------*
*& Form SELECT_EMPLOYEES
*&---------------------------------------------------------------------*
FORM select_employees.

  SELECT a~pernr a~ename a~bukrs a~werks a~btrtl a~kostl
         a~begda AS hire_date b~stat2
    INTO CORRESPONDING FIELDS OF TABLE gt_provisions
    FROM pa0001 AS a
    INNER JOIN pa0000 AS b ON a~pernr = b~pernr
    WHERE a~pernr IN s_pernr
      AND a~bukrs IN s_bukrs
      AND a~werks IN s_werks
      AND a~kostl IN s_kostl
      AND p_date BETWEEN a~begda AND a~endda
      AND p_date BETWEEN b~begda AND b~endda.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_SALARY_DATA
*&---------------------------------------------------------------------*
FORM get_salary_data.

  DATA: ls_pa0008 TYPE pa0008.

  LOOP AT gt_provisions INTO gs_provision.
*   Get basic salary from IT0008
    SELECT SINGLE * FROM pa0008 INTO ls_pa0008
      WHERE pernr = gs_provision-pernr
        AND p_date BETWEEN begda AND endda.

    IF sy-subrc = 0.
      gs_provision-basic_salary = ls_pa0008-ansal / 12.  " Monthly salary
    ENDIF.

    MODIFY gt_provisions FROM gs_provision.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form CALCULATE_PROVISIONS
*&---------------------------------------------------------------------*
FORM calculate_provisions.

  DATA: lv_daily_rate TYPE p DECIMALS 2.

  LOOP AT gt_provisions INTO gs_provision.
*   Get leave balance from absence quotas
*   This is placeholder - actual should read IT2006
    gs_provision-leave_balance = 21.  " Placeholder

*   Calculate daily rate (monthly salary / 30)
    lv_daily_rate = gs_provision-basic_salary / 30.

*   Calculate provision amount
    gs_provision-provision_amt = gs_provision-leave_balance * lv_daily_rate.

    MODIFY gt_provisions FROM gs_provision.
    gv_total_prov = gv_total_prov + gs_provision-provision_amt.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FILTER_RESULTS
*&---------------------------------------------------------------------*
FORM filter_results.

* Filter by active status
  IF p_active = 'X'.
    DELETE gt_provisions WHERE stat2 <> '3'.
  ENDIF.

* Remove zero provisions
  IF p_zero <> 'X'.
    DELETE gt_provisions WHERE provision_amt = 0.
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
          t_table      = gt_provisions ).

      lo_alv->display( ).

    CATCH cx_salv_msg.
      PERFORM display_simple.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_SIMPLE
*&---------------------------------------------------------------------*
FORM display_simple.

  WRITE: / 'Leave Provision Report'.
  WRITE: / 'Date:', p_date.
  ULINE.

  WRITE: / 'Pernr', 12 'Name', 35 'Status', 45 'Leave Bal', 58 'Provision'.
  ULINE.

  LOOP AT gt_provisions INTO gs_provision.
    WRITE: / gs_provision-pernr,
             12 gs_provision-ename,
             35 gs_provision-stat2,
             45 gs_provision-leave_balance,
             58 gs_provision-provision_amt.
  ENDLOOP.

  ULINE.
  WRITE: / 'Total Provision:', 58 gv_total_prov.

ENDFORM.
