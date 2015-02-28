MODULE INTERPOL
  USE HTYPE
  USE parameters
  USE GENESIS
  
  
  IMPLICIT NONE
CONTAINS
  !--------------------------------------------------!
  !              APPROXIMATING ROUTINES              !
  !               -LINEAR INTERP.                    !
  !--------------------------------------------------!


  !----------------------------------------------------------------------------!
  !             INTERPOLATING ROUTINES FOR SCALAR VALUES                       !
  !----------------------------------------------------------------------------!
  SUBROUTINE approx_sw(a, approx)
    TYPE(node), INTENT(IN) :: a
    REAL(DP), INTENT(INOUT) :: approx
    approx = a%fn_value
  ENDSUBROUTINE approx_sw



  SUBROUTINE approx_se(a, approx)
    TYPE(node), INTENT(IN) :: a
    REAL(DP), INTENT(INOUT) :: approx
    approx = interpolate3_2D(a  , a%x + .5_DP*a%h_value  ,  a%y )
  ENDSUBROUTINE approx_se



  SUBROUTINE approx_nw(a, approx)
    TYPE(node), INTENT(IN) :: a
    REAL(DP), INTENT(INOUT) :: approx
    approx = interpolate3_2D(a  , a%x  ,  a%y + .5_DP*a%h_value)
  ENDSUBROUTINE approx_nw


  SUBROUTINE approx_ne(a, approx)
    TYPE(node), INTENT(IN) :: a
    REAL(DP), INTENT(INOUT) :: approx
    
    approx = interpolate3_2D(a  , a%x + .5_DP*a%h_value  ,  a%y + .5_DP*a%h_value)
  ENDSUBROUTINE approx_ne


  FUNCTION interpolate3_2D(a, px, py)
    TYPE(node) :: a
    REAL(DP) :: px, py            !approximation point
    REAL(DP) :: interpolate3_2D


    REAL(DP) :: px1, px2, px3
    REAL(DP) :: py1, py2, py3
    REAL(DP) :: fx1, fx2, fx3


    !REQUIRED COUSINS FOR COMPUTATION OF Qx, Qy, Qxy
    REAL(DP) :: north, south, east, west
    REAL(DP) :: nw, ne, sw, se


    CALL fetch_cousins_scalar(a,north, south, east, west, nw, sw, se, ne)

    px1 = a%x - a%h_value ;    py1 = a%y - a%h_value
    px2 = a%x             ;    py2 = a%y
    px3 = a%x + a%h_value ;    py3 = a%y + a%h_value


    fx1 = interpolate3(px1,px2,px3, sw,south,se, px)
    fx2 = interpolate3(px1,px2,px3, west,a%fn_value,east, px)
    fx3 = interpolate3(px1,px2,px3, nw,north,ne, px)
    interpolate3_2D = interpolate3(py1,py2,py3, fx1,fx2,fx3, py)
  ENDFUNCTION interpolate3_2D


  SUBROUTINE fetch_cousins_scalar(a,north, south, east, west, nw, sw, se, ne)
    TYPE(node), INTENT(INOUT) :: a

    !REQUIRED COUSINS FOR COMPUTATION
    REAL(DP)  :: north, south, east, west
    REAL(DP)  :: nw, ne, sw, se

    ! FETCH WEST COUSIN
    IF(ASSOCIATED(a%w_cous)) THEN
       west = a%w_cous%fn_value
    ELSE
       IF(abs(a%y - a%parent%y)<zero)THEN
          CALL approx_se(a%parent%w_cous, west)
       ELSE
          CALL approx_ne(a%parent%w_cous, west)
       ENDIF
    ENDIF

    !FETCH EAST COUSIN
    IF( ASSOCIATED(a%e_cous)) THEN
       east = a%e_cous%fn_value
    ELSE
       IF(abs(a%y - a%parent%y)<zero)THEN
          CALL approx_sw(a%parent%e_cous, east)
       ELSE
          CALL approx_nw(a%parent%e_cous, east)
       ENDIF

    ENDIF


    ! FETCH NORTH COUSIN
    IF( ASSOCIATED( a%n_cous) ) THEN
       north = a%n_cous%fn_value
    ELSE
       IF(abs(a%x - a%parent%x)<zero)THEN
          CALL approx_sw(a%parent%n_cous, north)
       ELSE
          CALL approx_se(a%parent%n_cous, north)
       ENDIF
    ENDIF


    !FETCH SOUTH COUSIN
    IF( ASSOCIATED( a%s_cous) ) THEN
       south = a%s_cous%fn_value
    ELSE
       IF(ABS(a%x - a%parent%x)<zero) THEN
          CALL approx_nw(a%parent%s_cous, south)
       ELSE
          CALL approx_ne(a%parent%s_cous, south)
       ENDIF
    ENDIF


    !FETCH NORTH-EAST COUSIN

    IF(ABS(a%h_value - h_root)>ZERO)then
       IF(ABS(a%y - a%parent%y)>zero) THEN

          IF(ABS(a%x - a%parent%x)<ZERO)THEN
             IF(ASSOCIATED(a%parent%n_cous%se_child))THEN
                ne = a%parent%n_cous%se_child%fn_value
             ELSE
                CALL approx_se(a%parent%n_cous, ne)
             ENDIF
          ELSE
             IF(ASSOCIATED(a%parent%n_cous%e_cous%sw_child))THEN
                ne = a%parent%n_cous%e_cous%sw_child%fn_value
             ELSE
                CALL approx_sw(a%parent%n_cous%e_cous, ne)
             ENDIF
          ENDIF

       ELSE

          IF(ABS(a%x - a%parent%x)<ZERO)THEN
             ne = a%parent%ne_child%fn_value
          ELSE
             IF(ASSOCIATED(a%parent%e_cous%nw_child))THEN
                ne = a%parent%e_cous%nw_child%fn_value
             ELSE
                CALL approx_nw(a%parent%e_cous, ne)
             ENDIF
          ENDIF

       ENDIF
    ELSE
       ne = a%n_cous%e_cous%fn_value
    ENDIF


    !-----------------------------------------------------!
    !             FETCH SOUTH-EAST COUSIN
    !-----------------------------------------------------!
    IF(ABS(a%h_value - h_root)>ZERO)then
       IF(ABS(a%y - a%parent%y)>ZERO)THEN

          IF(ABS(a%x - a%parent%x)<ZERO)THEN
             se = a%parent%se_child%fn_value
          ELSE
             IF(ASSOCIATED(a%parent%e_cous%sw_child))THEN
                se = a%parent%e_cous%sw_child%fn_value
             ELSE
                CALL approx_sw(a%parent%e_cous, se)
             ENDIF
          ENDIF


       ELSE
          IF(ABS(a%x - a%parent%x)<ZERO)THEN
             IF( ASSOCIATED(a%parent%s_cous%ne_child))THEN
                se = a%parent%s_cous%ne_child%fn_value
             ELSE
                CALL approx_ne(a%parent%s_cous, se)
             ENDIF
          ELSE
             IF(ASSOCIATED(a%parent%s_cous%e_cous%nw_child))THEN
                se = a%parent%s_cous%e_cous%nw_child%fn_value
             ELSE
                CALL approx_nw(a%parent%s_cous%e_cous, se)
             ENDIF
          ENDIF

       ENDIF
    ELSE
       se = a%s_cous%e_cous%fn_value
    ENDIF


    ! FETCH SOUTH-WEST COUSIN
    IF(ABS(a%h_value - h_root)>ZERO)then
       IF(ABS(a%y - a%parent%y)>ZERO)THEN
          IF(ABS(a%x - a%parent%x)<ZERO)THEN
             IF(ASSOCIATED(a%parent%w_cous%se_child))THEN
                sw = a%parent%w_cous%se_child%fn_value
             ELSE
                CALL approx_se(a%parent%w_cous, sw)
             ENDIF
          ELSE
             sw = a%parent%sw_child%fn_value
          ENDIF


       ELSE

          IF(ABS(a%x-a%parent%x)<ZERO)THEN
             IF(ASSOCIATED(a%parent%s_cous%w_cous%ne_child))THEN
                sw = a%parent%s_cous%w_cous%ne_child%fn_value
             ELSE
                CALL approx_ne(a%parent%s_cous%w_cous, sw)
             ENDIF
          ELSE
             IF(ASSOCIATED(a%parent%s_cous%nw_child))THEN
                sw = a%parent%s_cous%nw_child%fn_value
             ELSE
                CALL approx_nw(a%parent%s_cous, sw)
             ENDIF
          ENDIF

       ENDIF
    ELSE
       sw = a%s_cous%w_cous%fn_value
    ENDIF


    ! FETCH NORTH-WEST COUSIN
    IF(ABS(a%h_value - h_root)>ZERO)then

       IF(ABS(a%y-a%parent%y)>ZERO)THEN

          IF(ABS(a%x-a%parent%x)<ZERO)THEN
             IF(ASSOCIATED(a%parent%n_cous%w_cous%se_child))THEN
                nw = a%parent%n_cous%w_cous%se_child%fn_value
             ELSE
                CALL approx_se(a%parent%n_cous%w_cous, nw)
             ENDIF
          ELSE
             IF(ASSOCIATED(a%parent%n_cous%sw_child)) THEN
                nw = a%parent%n_cous%sw_child%fn_value
             ELSE
                CALL approx_sw(a%parent%n_cous, nw)
             ENDIF
          ENDIF
       ELSE

          IF(ABS(a%x-a%parent%x)<ZERO)THEN
             IF(ASSOCIATED(a%parent%w_cous%ne_child)) THEN
                nw = a%parent%w_cous%ne_child%fn_value
             ELSE
                CALL approx_ne(a%parent%w_cous, nw)
             ENDIF
          ELSE
             nw = a%parent%nw_child%fn_value
          ENDIF

       ENDIF
    ELSE
       nw= a%n_cous%w_cous%fn_value
    ENDIF

  ENDSUBROUTINE fetch_cousins_scalar

ENDMODULE INTERPOL
