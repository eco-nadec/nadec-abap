*&---------------------------------------------------------------------*
*& Report Z_TEST_PROGRAM
*&---------------------------------------------------------------------*
REPORT z_test_program.

* Data declarations
DATA: lv_message TYPE string,
      lv_number  TYPE i,
      lv_total   TYPE i.

* Welcome message
lv_message = 'Welcome to ABAP Programming!'.
WRITE: / lv_message.
ULINE.

* Calculate sum of numbers 1 to 10
lv_total = 0.
DO 10 TIMES.
  lv_number = sy-index.
  lv_total = lv_total + lv_number.
ENDDO.

WRITE: / 'Sum of numbers 1 to 10:', lv_total.
SKIP.

* Check if number is even or odd
lv_number = 7.
IF lv_number MOD 2 = 0.
  WRITE: / lv_number, 'is even'.
ELSE.
  WRITE: / lv_number, 'is odd'.
ENDIF.
SKIP.

* Simple CASE statement
DATA: lv_day TYPE i VALUE 3.
CASE lv_day.
  WHEN 1.
    WRITE: / 'Monday'.
  WHEN 2.
    WRITE: / 'Tuesday'.
  WHEN 3.
    WRITE: / 'Wednesday'.
  WHEN OTHERS.
    WRITE: / 'Other day'.
ENDCASE.

WRITE: / 'Program completed!'.
