MODULE EVOLUTION
  USE HTYPE
  USE parameters
  USE INTERPOL
  USE GENESIS

CONTAINS

  RECURSIVE SUBROUTINE advection(a,u,v)
    TYPE(node), INTENT(INOUT), TARGET :: a
    REAL(DP), DIMENSION(0:N-1, 0:N-1) :: u,v

    INTEGER(DP) :: i,j
    REAL(DP) :: e_w, n_s

    !REQUIRED COUSINS FOR COMPUTATION
    REAL(DP) :: north, south, east, west
    REAL(DP) :: nw, ne, sw, se


    ! 1 - GO ALL THE WAY TO THE LEAVES
    IF(ASSOCIATED(a%ne_child))THEN
       CALL advection(a%ne_child,u,v)
       CALL advection(a%nw_child,u,v)
       CALL advection(a%se_child,u,v)
       CALL advection(a%sw_child,u,v)
    ELSE
       i = nint((a%x - x0)/h)
       j = nint((a%y - y0)/h)

       CALL fetch_cousins_scalar(a,north, south, east, west, nw, sw, se, ne)

       ! MAKE SURE TO UPWIND
       IF(u(i,j) .GE. 0 ) THEN
          e_w = west
          IF(v(i,j) .GE. 0)THEN
             n_s = south
             a%fn_new = -(dt/a%h_value)*(  u(i,j)*(a%fn_value - e_w) + &
                  v(i,j)*(a%fn_value - n_s) ) + a%fn_value
          ELSE
             n_s = north
             a%fn_new = -(dt/a%h_value)*(  u(i,j)*(a%fn_value - e_w) + &
                  v(i,j)*(n_s - a%fn_value) ) + a%fn_value
          ENDIF
       ELSE
          e_w = east
          IF(v(i,j) .GE. 0)THEN
             n_s = south
             a%fn_new = -(dt/a%h_value)*(  u(i,j)*(e_w - a%fn_value) + &
                  v(i,j)*(a%fn_value - n_s) ) + a%fn_value
          ELSE
             n_s = north
             a%fn_new = -(dt/a%h_value)*(  u(i,j)*(e_w - a%fn_value) + &
                  v(i,j)*(n_s - a%fn_value) ) + a%fn_value
          ENDIF
       ENDIF
    ENDIF

  ENDSUBROUTINE advection


ENDMODULE EVOLUTION
