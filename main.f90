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
  REAL(DP), DIMENSION(0:N-1, 0:N-1):: u0, u, v0, vnew


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
     print*, time, time*dt
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

     ! WRITE THE VALUES FROM THE TREE


     IF(mod(time,int(tf/dt)/100) == 0)THEN
        if(fcount < 10)then
           WRITE(filename, "(I1,A4)") fcount,'.txt'
        else
           WRITE(filename, "(I2,A4)") fcount,'.txt'
        endif
        print*, 'writing to file', filename
        OPEN(44, file = "./data/"//trim(filename), status = 'replace')
        y_temp => y_root_head
        DO
           x_temp => y_temp
           DO
              CALL write_fn_value(x_temp,44)
              IF(x_temp%x+x_temp%h_value == xf) EXIT
              x_temp => x_temp%e_cous
           ENDDO
           IF(y_temp%y+y_temp%h_value == yf) EXIT
           y_temp => y_temp%n_cous
        ENDDO
        CLOSE(44)
        fcount = fcount + 1
     ENDIF
  ENDDO



!!$  ! FOR EACH TIME STEP,
!!$  OPEN(10, FILE = "trace.txt")
!!$  OPEN(11, file = "mean_x_position.txt", status = 'replace')
!!$  OPEN(12, FILE = "extension.txt")
!!$  OPEN(14, FILE = "time_cost.txt")
!!$
!!$  OPEN(20, FILE = "boundary_various_times.txt")
!!$  OPEN(21, FILE = "stress_various_times.txt")
!!$  DO time = 1,int(tf/dt)
!!$     call cpu_time(start_time)
!!$
!!$     CALL Mn_MATRIX(T1n, T2n, U1n, U2n, T1,T2, U1,U2, bdry0)
!!$     Mn(0:Nb, 0:Nb) = T1n
!!$     Mn(Nb+1:2*Nb+1, 0:Nb) = T2n
!!$     Mn(0:Nb, Nb+1:2*Nb+1) = U1n
!!$     Mn(Nb+1:2*Nb+1, Nb+1:2*Nb+1) = U2n
!!$
!!$
!!$     !-------------------------------------------------!
!!$     !                  UPDATE BOUNDARY                !
!!$     !                  UPDATE FLUID                   !
!!$     !-------------------------------------------------!
!!$     CALL Bn_VECTOR(Bn, u0, v0, bdry0, p_stress_u, p_stress_v)
!!$
!!$     IF(time .GE. int(18./dt)) previous_position = bdry0
!!$
!!$     CALL NEWTON(bdry0, bdry, time*dt, Mn, Bn)
!!$     bdry0 = bdry
!!$     CALL FLUID_SOLVE(u0, v0, bdry, time*dt,p_stress_u, p_stress_v)
!!$
!!$
!!$
!!$     !-------------------------------------------------!
!!$     !          UPDATE STRESS TENSOR FIELD             !
!!$     !-------------------------------------------------!
!!$
!!$     !---------------------------- FOR EACH LEAF IN TREE -------------------------------------!
!!$     !  I.     SOLVE FOR p_stress_tensor - this will require u0,v0, p_stress_tensor 
!!$     !         UPDATE THE p_scalar_field value (a%fn_value) WHILE HERE.
!!$     !  II.    INTERPOLATE/RECOVER THE FULL p_stress_tensor FROM THE 
!!$     !         new_p_stress_tensor AT THE LEAVES
!!$     !  III.   PERFORM THE MR ANALYSIS ON THE TREE
!!$     !----------------------------------------------------------------------------------------!
!!$
!!$     ! STEP I.a - update the leaves via fene
!!$     y_temp => y_root_head
!!$     DO
!!$        x_temp => y_temp
!!$        DO
!!$           CALL final_step(x_temp,u0,v0, Id, MAT1, MAT2, MAT3, K_mat, L_mat, M_mat)
!!$
!!$           IF(x_temp%x+x_temp%h_value == xf) EXIT
!!$           x_temp => x_temp%e_cous
!!$        ENDDO
!!$        IF(y_temp%y+y_temp%h_value == yf) EXIT
!!$        y_temp => y_temp%n_cous
!!$     ENDDO
!!$
!!$
!!$     ! STEP I.b - peform the hyperbolic part
!!$     y_temp => y_root_head
!!$     DO
!!$        x_temp => y_temp
!!$        DO
!!$           CALL hyperbolic_step(x_temp,u0,v0) 
!!$
!!$           IF(x_temp%x+x_temp%h_value == xf) EXIT
!!$           x_temp => x_temp%e_cous
!!$        ENDDO
!!$        IF(y_temp%y+y_temp%h_value == yf) EXIT
!!$        y_temp => y_temp%n_cous
!!$     ENDDO
!!$
!!$
!!$
!!$     ! STEP I.b - peform the projection
!!$     y_temp => y_root_head
!!$     DO
!!$        x_temp => y_temp
!!$        DO
!!$           ! project the new fn_value, stress tensor, coeffs
!!$           ! and switch the coeffs at the leaves
!!$           CALL final_projection(x_temp)
!!$           IF(x_temp%x+x_temp%h_value == xf) EXIT
!!$           x_temp => x_temp%e_cous
!!$        ENDDO
!!$        IF(y_temp%y+y_temp%h_value == yf) EXIT
!!$        y_temp => y_temp%n_cous
!!$     ENDDO
!!$
!!$
!!$     ! STEP II. - recover the full stress_tensor via interpolation
!!$     y_temp => y_root_head
!!$     DO
!!$        x_temp => y_temp
!!$        DO
!!$           CALL recover_stress(x_temp,p_stress_tensor)
!!$           IF(x_temp%x+x_temp%h_value == xf) EXIT
!!$           x_temp => x_temp%e_cous
!!$        ENDDO
!!$
!!$        IF(y_temp%y+y_temp%h_value == yf) EXIT
!!$        y_temp => y_temp%n_cous
!!$     ENDDO
!!$
!!$     !--------------------------------------------!
!!$     !       SET DETAILS/MARK AS DELETABLE        !
!!$     !--------------------------------------------!
!!$     y_temp => y_root_head
!!$     DO
!!$        x_temp => y_temp
!!$        DO
!!$           !           CALL details(x_temp)
!!$           CALL coeff_details(x_temp)
!!$           IF(x_temp%x+x_temp%h_value == xf) EXIT
!!$           x_temp => x_temp%e_cous
!!$        ENDDO
!!$        IF(y_temp%y+y_temp%h_value == yf) EXIT
!!$        y_temp => y_temp%n_cous
!!$     ENDDO
!!$
!!$     ! WRITE THE VALUES FROM THE TREE
!!$     OPEN(44, file = "tree_values4.txt",status = 'replace')
!!$     y_temp => y_root_head
!!$     DO
!!$        x_temp => y_temp
!!$        DO
!!$           CALL write_s_tensor(x_temp,44)
!!$           IF(x_temp%x+x_temp%h_value == xf) EXIT
!!$           x_temp => x_temp%e_cous
!!$        ENDDO
!!$        IF(y_temp%y+y_temp%h_value == yf) EXIT
!!$        y_temp => y_temp%n_cous
!!$     ENDDO
!!$     CLOSE(44)
!!$
!!$     !------------------------------------------------!
!!$     !                REMOVE/ADD NODES                !
!!$     !------------------------------------------------!
!!$     y_temp => y_root_head
!!$     DO
!!$        x_temp => y_temp
!!$        DO
!!$           CALL removal(x_temp)
!!$           CALL reconstruct(x_temp)
!!$           IF(x_temp%x+x_temp%h_value == xf) EXIT
!!$           x_temp => x_temp%e_cous
!!$        ENDDO
!!$        IF(y_temp%y+y_temp%h_value == yf) EXIT
!!$        y_temp => y_temp%n_cous
!!$     ENDDO
!!$
!!$
!!$
!!$     !-------------------------------------------------!
!!$     !           CALCULATE THE STRESS FIELD            !
!!$     !-------------------------------------------------!
!!$     DO i = 0, N-1
!!$        DO j = 0, N-1
!!$           p_stress_u(i,j) = &
!!$                ( p_stress_tensor(MODULO(i+1,N),j,0,0) - p_stress_tensor(MODULO(i-1,N),j,0,0) &
!!$                + p_stress_tensor(i,MODULO(j+1,N),0,1) - p_stress_tensor(i,MODULO(j-1,N),0,1) )/(2*h)
!!$           p_stress_v(i,j) = &
!!$                ( p_stress_tensor(MODULO(i+1,N),j,1,0) - p_stress_tensor(MODULO(i-1,N),j,1,0) &
!!$                + p_stress_tensor(i,MODULO(j+1,N),1,1) - p_stress_tensor(i,MODULO(j-1,N),1,1) )/(2*h)
!!$        ENDDO
!!$     ENDDO
!!$
!!$
!!$
!!$     OPEN(13, FILE = "bdry.txt")
!!$     DO s= 0, Nb
!!$        write(13,"(2(1x,f15.9))") bdry(s), bdry(s+Nb+1)
!!$     ENDDO
!!$     CLOSE(13)
!!$
!!$
!!$     !-----COMPUTE THE TIME AVERAGED POWER-----!
!!$     IF(time .ge. int(18./dt))THEN
!!$        bdry_velocity = bdry - previous_position !NOTE THAT WE DO NOT DIVIDE BY dt SINCE THE
!!$        !THE INTEGRAL IN TIME MULTIPLIES BY dt
!!$        bdry_x_displacement = bdry_x_displacement &
!!$             + sum(bdry(0:Nb) - previous_position(0:Nb))/real(Nb+1) !AVERAGE X DISPLACEMENT
!!$
!!$        CALL FORCE(AX, bdry, time*dt)
!!$        power = power + dot_product(AX, bdry_velocity)*hb
!!$
!!$        CALL FORCE(AX, previous_position, time*dt)
!!$        power2 = power2 + dot_product(AX, bdry_velocity)*hb
!!$     ENDIF
!!$
!!$
!!$     IF(  time == int(5/dt)  .OR. time == int(10/dt)  .OR. &
!!$          time == int(15/dt) .OR. time == int(19.5/dt)   )THEN
!!$
!!$        write(20,"(A4 , I8)") "time", time
!!$        DO s= 0, Nb
!!$           write(20,"(2(1x,f15.9))") bdry(s), bdry(s+Nb+1)
!!$        ENDDO
!!$
!!$
!!$        write(21,"(A4 , I8)") "time", time
!!$        DO i = int(.3/h), int(1.6/h)
!!$           DO j = int(.8/h), int(1.1/h)
!!$              write(21,"(9(1x,f15.9))") i*h, j*h, p_stress_u(i,j), p_stress_v(i,j),&
!!$                   p_stress_tensor(i,j,0,0), p_stress_tensor(i,j,0,1), p_stress_tensor(i,j,1,0),&
!!$                   p_stress_tensor(i,j,1,1), p_stress_tensor(i,j,0,0) + p_stress_tensor(i,j,1,1)
!!$           ENDDO
!!$        ENDDO
!!$
!!$     ENDIF
!!$
!!$
!!$     write(10,"(5(1x,f15.9))") time*dt, bdry(0), bdry(Nb+1), bdry(Nb/2), bdry(Nb/2 + Nb+1)
!!$     write(11,"(2(1x,f15.9))") time*dt, sum(bdry(0:Nb))/real(Nb+1)
!!$
!!$     call cpu_time(end_time)
!!$     
!!$     write(14,"(2(1x,f15.9))") time*dt, (start_time - end_time)
!!$
!!$
!!$  ENDDO !TIME
!!$
!!$  CLOSE(10)
!!$  CLOSE(14)
!!$  CLOSE(11)
!!$  CLOSE(12)
!!$  CLOSE(20)
!!$  CLOSE(21)
!!$
!!$  OPEN(10, FILE = "power.txt")
!!$  write(10,"(2(1x,f15.9))")  De, power/2., power2/2.
!!$  CLOSE(10)
!!$
!!$
!!$  OPEN(10, FILE = "efficiency.txt")
!!$  write(10,"(2(1x,f15.9))")  De, (bdry_x_displacement*.5)**2/(.5*power)
!!$  CLOSE(10)
!!$
!!$
!!$  OPEN(13, FILE = "total_stress.txt")
!!$  DO i = 0,N-1
!!$     DO j = 0,N-1
!!$
!!$        write(13,"(9(1x,f15.9))") i*h, j*h, p_stress_u(i,j), p_stress_v(i,j),&
!!$             p_stress_tensor(i,j,0,0), p_stress_tensor(i,j,0,1), p_stress_tensor(i,j,1,0),&
!!$             p_stress_tensor(i,j,1,1), p_stress_tensor(i,j,0,0) + p_stress_tensor(i,j,1,1)
!!$     ENDDO
!!$  ENDDO
!!$  CLOSE(13)

ENDPROGRAM main
