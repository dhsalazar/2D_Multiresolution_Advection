PROGRAM main
  USE HTYPE
  USE parameters
  USE GENESIS
  USE EVOLUTION
  USE MR


  IMPLICIT NONE

  !-----------------------------------------------------------!
  !                    FLUID VELOCITIES                       !
  !-----------------------------------------------------------!
  REAL(DP), DIMENSION(0:N-1, 0:N-1):: u0, v0


  !-----------------------------------------------------------!
  !                       TREE POINTERS                       !
  !-----------------------------------------------------------!
  TYPE(node), POINTER :: x_root_head, x_root_tail, x_temp
  TYPE(node), POINTER :: y_root_head, y_root_tail, y_temp

  !-----------------------------------------------------------!
  !                       VARIOUS INDECES                     !
  !-----------------------------------------------------------!
  REAL(DP) :: x,y, extension
  INTEGER(DP) :: i,j, s, left, right
  INTEGER(DP) :: time
  REAL(DP) :: start_time, end_time
  REAL(DP), PARAMETER :: tf = 2
  character(6) :: filename
  integer :: fcount = 1
  ! DEFINE INITAL VELOCITY
  u0 = 1.0
  v0 = 0.0

  !---------------------- CREATE INITIAL TREE--------------------------!
  CALL create_root(x_root_head, x_root_tail, y_root_head, y_root_tail)
  y_temp => y_root_head
  DO
     x_temp => y_temp
     DO
        CALL initial_check(x_temp)
        IF(x_temp%x+x_temp%h_value == xf) EXIT
        x_temp => x_temp%e_cous
     ENDDO
     IF(y_temp%y+y_temp%h_value == yf) EXIT
     y_temp => y_temp%n_cous
  ENDDO



  !--------------------------------------------------------------------!
  !                       START TIME ITERATION                         !
  !--------------------------------------------------------------------!


  DO time = 1,int(tf/dt)
     !print*, time, time*dt
     ! peform the advection update
     y_temp => y_root_head
     DO
        x_temp => y_temp
        DO
           CALL advection(x_temp,u0,v0) 

           IF(x_temp%x+x_temp%h_value == xf) EXIT
           x_temp => x_temp%e_cous
        ENDDO
        IF(y_temp%y+y_temp%h_value == yf) EXIT
        y_temp => y_temp%n_cous
     ENDDO

     ! STEP I.b - peform the projection
     y_temp => y_root_head
     DO
        x_temp => y_temp
        DO
           CALL update_leaves(x_temp)
           IF(x_temp%x+x_temp%h_value == xf) EXIT
           x_temp => x_temp%e_cous
        ENDDO
        IF(y_temp%y+y_temp%h_value == yf) EXIT
        y_temp => y_temp%n_cous
     ENDDO

     ! STEP I.b - peform the projection
     y_temp => y_root_head
     DO
        x_temp => y_temp
        DO
           CALL scalar_projection(x_temp)
           IF(x_temp%x+x_temp%h_value == xf) EXIT
           x_temp => x_temp%e_cous
        ENDDO
        IF(y_temp%y+y_temp%h_value == yf) EXIT
        y_temp => y_temp%n_cous
     ENDDO

     !--------------------------------------------!
     !       SET DETAILS/MARK AS DELETABLE        !
     !--------------------------------------------!
     y_temp => y_root_head
     DO
        x_temp => y_temp
        DO
           CALL details(x_temp)
           IF(x_temp%x+x_temp%h_value == xf) EXIT
           x_temp => x_temp%e_cous
        ENDDO
        IF(y_temp%y+y_temp%h_value == yf) EXIT
        y_temp => y_temp%n_cous
     ENDDO

     !------------------------------------------------!
     !                REMOVE/ADD NODES                !
     !------------------------------------------------!
     y_temp => y_root_head
     DO
        x_temp => y_temp
        DO
           CALL removal(x_temp)
           CALL reconstruct(x_temp)
           IF(x_temp%x+x_temp%h_value == xf) EXIT
           x_temp => x_temp%e_cous
        ENDDO
        IF(y_temp%y+y_temp%h_value == yf) EXIT
        y_temp => y_temp%n_cous
     ENDDO

!!$     ! WRITE THE VALUES FROM THE TREE
!!$     ! - writes values to local directory './data'
!!$     IF(mod(time,int(tf/dt)/100) == 0)THEN
!!$        if(fcount < 10)then
!!$           WRITE(filename, "(I1,A4)") fcount,'.txt'
!!$        else
!!$           WRITE(filename, "(I2,A4)") fcount,'.txt'
!!$        endif
!!$        print*, 'writing to file', filename
!!$        OPEN(44, file = "./data/"//trim(filename), status = 'replace')
!!$        y_temp => y_root_head
!!$        DO
!!$           x_temp => y_temp
!!$           DO
!!$              CALL write_fn_value(x_temp,44)
!!$              IF(x_temp%x+x_temp%h_value == xf) EXIT
!!$              x_temp => x_temp%e_cous
!!$           ENDDO
!!$           IF(y_temp%y+y_temp%h_value == yf) EXIT
!!$           y_temp => y_temp%n_cous
!!$        ENDDO
!!$        CLOSE(44)
!!$        fcount = fcount + 1
!!$     ENDIF

  ENDDO

ENDPROGRAM main
