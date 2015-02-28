MODULE GENESIS

  USE HTYPE
  USE parameters

  IMPLICIT NONE
  TYPE ::  node
     
     REAL(DP) :: fn_value
     REAL(DP) :: fn_new
     REAL(DP) :: x, y
     REAL(DP) :: h_value
     
     REAL(DP) :: detail
     
     TYPE(node), POINTER :: parent
     
     !-----------------------------------------------------------------!
     !        CHILDREN ARE ASSOCIATED WITH THE FOUR CORNERS            !
     !        OF THE SQUARE USING N,S,E,W                              !
     !-----------------------------------------------------------------!
     TYPE(node), POINTER :: nw_child, ne_child, sw_child, se_child
     
     !-----------------------------------------------------------------!
     !               COUSINS FOR EACH SIDE OF THE NODE                 !
     !-----------------------------------------------------------------!
     TYPE(node), POINTER :: n_cous, s_cous, e_cous, w_cous
     
     
     LOGICAL :: small_detail, deletable    
  ENDTYPE node
  
CONTAINS
  
  
  SUBROUTINE create_root(x_head, x_tail, y_head, y_tail)
    
    TYPE(node), POINTER :: x_head, x_tail, y_head, y_tail

    TYPE(node), POINTER :: x_temp, y_temp
    TYPE(node), POINTER :: ns_linker !used to link north/south cousins
    
    INTEGER :: x_count, y_count
    
    ! STEP 1: CREATE THE FIRST ROW OF NODES

    ! 1.1 - ALLOCATE VERY FIRST NODE FOR BOTH-LEFT CORNER
    ALLOCATE(y_head)
    NULLIFY(y_head%sw_child,y_head%nw_child, y_head%se_child, y_head%ne_child)
    NULLIFY(y_head%w_cous,y_head%e_cous,y_head%n_cous, y_head%s_cous)
    NULLIFY(y_head%parent)
    

    
    ! 1.2 - SET small_detail AND deletable TO TRUE
    y_head%small_detail = .true.
    y_head%deletable = .true.
    y_head%detail = 0.0
    
    
    ! 1.3 - SET x_value, y_value, h_value
    y_head%h_value = h_root
    y_head%x = x0
    y_head%y = y0
    
    
    ! 1.4 - SET fn_value 
    y_head%fn_value = initial_data(y_head%x, y_head%y)

    y_tail => y_head    
    x_temp => y_head
    x_head => y_head


    ! 1.5 - CREATE THE LIST ALONG THE X-DIRECTION
    DO x_count = 1,N_root-1

       !------------------------------------------------!
       ! 1: ALLOCATE ONE MORE NODE TO THE RIGHT         !
       !    AND NULLIFY ALL POINTERS IN NEW NODE        !
       !------------------------------------------------!
       
       ALLOCATE(x_temp%e_cous)
       
       x_tail => x_temp%e_cous

       NULLIFY(x_tail%sw_child, x_tail%se_child, x_tail%nw_child, x_tail%ne_child)
       NULLIFY(x_tail%n_cous, x_tail%s_cous, x_tail%e_cous,x_tail%w_cous)
       NULLIFY(x_tail%parent)

       !------------------------------------------------!
       ! 2: SET small_detail AND deletable TO TRUE      !
       !------------------------------------------------!

       x_tail%small_detail = .true.
       x_tail%deletable = .true.
       x_tail%detail = 0.0

       !------------------------------------------------!       
       ! 3: SET x_value, y_value, h_value               !
       !------------------------------------------------!
       x_tail%h_value = h_root
       
       x_tail%x = x0 + x_count*h_root
       x_tail%y = y_tail%y

       !------------------------------------------------!       
       ! 4: SET fn_value                                !
       !------------------------------------------------!

       x_tail%fn_value = initial_data(x_tail%x, x_tail%y)


       !------------------------------------------------!       
       ! 5: POINT x_tail%w_cous to x_temp               !
       !------------------------------------------------!
       x_tail%w_cous => x_temp

       !------------------------------------------------!       
       ! 5: POINT x_temp TO x_tail                      !
       !------------------------------------------------!

       x_temp => x_tail

       !--------------- X-PERIODICITY ----------------!
       IF(x_count == N_root-1) THEN
          x_tail%e_cous => x_head
          x_head%w_cous => x_tail
       ENDIF
       !----------------------------------------------!

    ENDDO
    
    !--------------------------------------------------------------!
    ! STEP 2: NOW THAT WE HAVE THE FIRST ROW, WE CAN BUILD         !
    !         THE REST OF THE ROOT-LATTICE                         !
    !--------------------------------------------------------------!
    
    y_temp => y_head
    
    DO y_count = 1, N_root-1
       
       ! 1 - ALLOCATE NEW Y-NODE:  THIS WILL SERVE AS THE
       !     BASE FOR THE NEW X-ROW.
       ALLOCATE(y_temp%n_cous)

       y_tail=> y_temp%n_cous

       NULLIFY(y_tail%sw_child,y_tail%nw_child, y_tail%se_child, y_tail%ne_child)
       NULLIFY(y_tail%w_cous,y_tail%e_cous,y_tail%n_cous,y_tail%s_cous)
       NULLIFY(y_tail%parent)
       
       
       ! 2 - SET small_detail AND deletable TO TRUE
       y_tail%small_detail = .true.
       y_tail%deletable = .true.
       y_tail%detail = 0.0
       
       ! 3 - SET x_value, y_value, h_value
       y_tail%h_value = h_root       
       y_tail%y = y0 + y_count*h_root
       y_tail%x = x0
       
       ! SET fn_value
       y_tail%fn_value = initial_data(y_tail%x, y_tail%y)
       
       ! POINT y_tail%s_cous => y_temp
       y_tail%s_cous => y_temp
       
       ! 4 - POINT y_temp => y_tail
       y_temp => y_tail
       
       
       
       ! TO CREATE THE X-ROW FROM THIS NEW NODE IN THE Y-DIRECTION
       ! FIRST POINT x_temp => y_tail
       x_temp => y_tail

       ns_linker => y_tail%s_cous !used to link north-south cousins
       
       DO x_count = 1, N_root-1
          
          ! 1: ALLOCATE ONE MORE NODE TO THE RIGHT
          !         AND NULLIFY ALL POINTERS IN NEW NODE
          
          ALLOCATE(x_temp%e_cous)
          x_tail => x_temp%e_cous
          NULLIFY(x_tail%sw_child, x_tail%se_child, x_tail%nw_child, x_tail%ne_child)
          NULLIFY(x_tail%n_cous, x_tail%s_cous, x_tail%e_cous,x_tail%w_cous)
          NULLIFY(x_tail%parent)
          
          ! MOVE ns_linker ONE NODE TO THE EAST
          ns_linker => ns_linker%e_cous
          
          ! 2: SET small_detail AND deletable TO TRUE
          x_tail%small_detail = .true.
          x_tail%deletable = .true.
          x_tail%detail = 0.0
          
          ! 3: SET x_value, y_value, h_value
          x_tail%h_value = h_root          
          x_tail%x = x0 + x_count*h_root
          x_tail%y = y_tail%y

          ! 4: SET fn_value
          x_tail%fn_value = initial_data(x_tail%x, x_tail%y)

          
          ! POINT x_tail%w_cous TO x_temp
          x_tail%w_cous => x_temp
          
          
          ! LINK WITH ITS BROTHER TO THE SOUTH
          ns_linker%n_cous => x_tail
          x_tail%s_cous => ns_linker
          
          ! 5: POINT x_temp TO x_tail
          x_temp => x_tail

          !--------------- X-PERIODICITY ----------------!
          IF(x_count == N_root-1) THEN
             x_tail%e_cous => y_tail
             y_tail%w_cous => x_tail
          ENDIF
          !----------------------------------------------!
       ENDDO
    ENDDO


    
    !---------------------------!
    !     Y - PERIODICITY       !
    ns_linker => y_head
    DO x_count = 0, N_root-1
       ns_linker%s_cous => y_temp
       y_temp%n_cous => ns_linker
       
       ns_linker => ns_linker%e_cous
       y_temp => y_temp%e_cous
    ENDDO
    !rmk:  if i'm not mistaken, y_temp and ns_linker should end up
    !      back where they started, pointing at y_tail and y_head.
  ENDSUBROUTINE create_root
  
!----------------------------------------------------------------------------!      
  
  
  RECURSIVE SUBROUTINE split_node(a)
    TYPE(node), INTENT(INOUT), TARGET :: a
    
    REAL(DP) :: x_west, x_east
    REAL(DP) :: y_north, y_south

    !---------------------------------------------------------------!
    ! THE FOLLOWING SUBROUTINE SHOULD INTAKE A NODE a, AND          !
    ! 1. ALLOCATE ALL 4 CHILDREN                                    !
    ! 2. SET THE APPROPRIATE VALUES IN THE FOUR CHILDREN            !
    ! 3. CHECK THAT ALL UNCLES ARE PRESENT - NOTE THAT THIS         !
    !    DEPENDS ON THE INTERPOLATION PROCEDURE BEING USED          !
    ! 4. LINK EVERYTHING TOGETHER CORRECTLY                         !
    !---------------------------------------------------------------!
    
    x_west = a%x
    x_east = a%x + a%h_value*.5_DP
    
    y_south = a%y 
    y_north = a%y + a%h_value*.5_DP


    ALLOCATE(a%nw_child)
    NULLIFY(a%nw_child%nw_child, a%nw_child%ne_child, &
         a%nw_child%sw_child, a%nw_child%se_child)
    NULLIFY(a%nw_child%n_cous,a%nw_child%s_cous,&
         a%nw_child%e_cous, a%nw_child%w_cous)
    NULLIFY(a%nw_child%parent)

    ALLOCATE(a%sw_child)
    NULLIFY(a%sw_child%nw_child, a%sw_child%ne_child, &
         a%sw_child%sw_child, a%sw_child%se_child)
    NULLIFY(a%sw_child%n_cous,a%sw_child%s_cous,&
         a%sw_child%e_cous, a%sw_child%w_cous)
    NULLIFY(a%sw_child%parent)

    ALLOCATE(a%ne_child)
    NULLIFY(a%ne_child%nw_child, a%ne_child%ne_child, &
         a%ne_child%sw_child, a%ne_child%se_child)
    NULLIFY(a%ne_child%n_cous,a%ne_child%s_cous,&
         a%ne_child%e_cous, a%ne_child%w_cous)
    NULLIFY(a%ne_child%parent)

    ALLOCATE(a%se_child)
    NULLIFY(a%se_child%nw_child, a%se_child%ne_child, &
         a%se_child%sw_child, a%se_child%se_child)
    NULLIFY(a%se_child%n_cous,a%se_child%s_cous,&
         a%se_child%e_cous, a%se_child%w_cous)
    NULLIFY(a%se_child%parent)

    !-------------------------------------------!
    ! 2 - SET ALL OF THE APPROPRIATE VALUES     !
    !-------------------------------------------!

    ! 2.1 - a%nw_child
    a%nw_child%x = x_west
    a%nw_child%y = y_north
    
    a%nw_child%h_value = a%h_value*.5_DP

    a%nw_child%fn_value = initial_data(a%nw_child%x, a%nw_child%y)


    ! 2.2 - a%sw_child
    a%sw_child%x = x_west
    a%sw_child%y = y_south
    
    a%sw_child%h_value = a%h_value*.5_DP

    a%sw_child%fn_value = initial_data(a%sw_child%x, a%sw_child%y)


    ! 2.3 - a%ne_child
    a%ne_child%x = x_east
    a%ne_child%y = y_north

    a%ne_child%h_value = a%h_value*.5_DP

    a%ne_child%fn_value = initial_data(a%ne_child%x, a%ne_child%y)


    ! 2.4 - a%se_child
    a%se_child%x = x_east
    a%se_child%y = y_south

    a%se_child%h_value = a%h_value*.5_DP

    a%se_child%fn_value = initial_data(a%se_child%x, a%se_child%y)

    !-------------------------------------------!
    ! 3 - LINK THE SIBLINGS TOGETHER            !
    !-------------------------------------------!    
    
    ! a%nw - east & south
    ! a%sw - east & north
    ! a%ne - west & south
    ! a%se - west & north
    
    a%nw_child%e_cous => a%ne_child
    a%nw_child%s_cous => a%sw_child
    
    a%sw_child%e_cous => a%se_child
    a%sw_child%n_cous => a%nw_child
    
    a%ne_child%w_cous => a%nw_child
    a%ne_child%s_cous => a%se_child
    
    a%se_child%w_cous => a%sw_child
    a%se_child%n_cous => a%ne_child

    !-------------------------------------------!
    ! 4 - POINT THEM TO PARENT                  !
    !-------------------------------------------!
    a%nw_child%parent => a
    a%ne_child%parent => a
    a%sw_child%parent => a
    a%se_child%parent => a
    

    !-------------------------------------------!
    ! 5 - CHECK FOR UNCLES                      !
    !   - THIS WILL DEPEND ON THE INTERP. USED  !
    !   - FOR NOW, WE USE LINEAR INTERP.        !
    !-------------------------------------------!    
    
    ! NEED TO MAKE SURE a%w_cous AND a%s_cous
    ! ARE AVAILABLE (OBSERVING PERIODICITY)

    IF(.NOT. ASSOCIATED(a%w_cous)) THEN
       CALL split_node(a%parent%w_cous)
    ENDIF
     
    IF(.NOT. ASSOCIATED(a%s_cous)) THEN
       CALL split_node(a%parent%s_cous)
    ENDIF
    
    IF(.NOT. ASSOCIATED(a%e_cous))THEN
       CALL split_node(a%parent%e_cous)
    ENDIF
    
    IF(.NOT. ASSOCIATED(a%n_cous))THEN
       CALL split_node(a%parent%n_cous)
    ENDIF
    
    !nw_cous
    IF(.NOT. ASSOCIATED(a%w_cous%n_cous)) THEN
       CALL split_node(a%parent%w_cous%n_cous)
    ENDIF

    !ne_cous
    IF(.NOT. ASSOCIATED(a%e_cous%n_cous)) THEN
       CALL split_node(a%parent%e_cous%n_cous)
    ENDIF
    
    !sw_cous
    IF(.NOT. ASSOCIATED(a%w_cous%s_cous)) THEN
       CALL split_node(a%parent%w_cous%s_cous)
    ENDIF

    !se_cous
    IF(.NOT. ASSOCIATED(a%e_cous%s_cous)) THEN
       CALL split_node(a%parent%e_cous%s_cous)
    ENDIF


    !-------------------------------------------!
    ! 6 - LINK ANY COUSINS WITH DIFFERENT       !
    !     PARENTS                               !
    !-------------------------------------------!

    ! 6.1 - LOOK TO THE WEST
    IF( ASSOCIATED( a%w_cous ))THEN
       IF ( ASSOCIATED(a%w_cous%ne_child))THEN

          a%nw_child%w_cous => a%w_cous%ne_child
          a%w_cous%ne_child%e_cous => a%nw_child
          
          a%sw_child%w_cous => a%w_cous%se_child
          a%w_cous%se_child%e_cous => a%sw_child
          
       ENDIF
    ENDIF
    
    ! 6.2 - LOOK TO THE NORTH
    IF( ASSOCIATED( a%n_cous))THEN
       IF( ASSOCIATED( a%n_cous%ne_child ))THEN
          
          a%ne_child%n_cous => a%n_cous%se_child
          a%n_cous%se_child%s_cous => a%ne_child
         
          a%nw_child%n_cous => a%n_cous%sw_child
          a%n_cous%sw_child%s_cous => a%nw_child
          
       ENDIF
    ENDIF
    
    
    ! 6.3 - LOOK TO THE EAST
    IF( ASSOCIATED( a%e_cous) ) THEN
       IF( ASSOCIATED( a%e_cous%ne_child ))THEN
          
          a%ne_child%e_cous => a%e_cous%nw_child
          a%e_cous%nw_child%w_cous => a%ne_child
          
          a%se_child%e_cous => a%e_cous%sw_child
          a%e_cous%sw_child%w_cous => a%se_child
          
       ENDIF
    ENDIF
    
    
    ! 6.4 - LOOK TO THE SOUTH
    IF( ASSOCIATED( a%s_cous )) THEN
       IF( ASSOCIATED( a%s_cous%ne_child))THEN
          
          a%sw_child%s_cous => a%s_cous%nw_child
          a%s_cous%nw_child%n_cous => a%sw_child
          
          a%se_child%s_cous => a%s_cous%ne_child
          a%s_cous%ne_child%n_cous => a%se_child
          
       ENDIF
    ENDIF

  ENDSUBROUTINE split_node
  

  FUNCTION approx_nw0(a)
    TYPE(node) :: a
    REAL(DP) :: approx_nw0
    
    REAL(DP) :: px, py

    px = a%x
    py = a%y + a%h_value*.5_DP
    approx_nw0 = interpolate3_2D_initial(a,px,py)

  ENDFUNCTION approx_nw0



  FUNCTION approx_se0(a)
    TYPE(node) :: a
    REAL(DP) :: approx_se0

    REAL(DP) :: px, py

    px = a%x + a%h_value*.5_DP
    py = a%y 
    approx_se0 = interpolate3_2D_initial(a,px,py)
  ENDFUNCTION approx_se0


  
  FUNCTION approx_sw0(a)
    TYPE(node) :: a
    REAL(DP) :: approx_sw0
    
    REAL(DP) :: px, py

    px = a%x
    py = a%y 
    approx_sw0 = interpolate3_2D_initial(a,px,py)
  ENDFUNCTION approx_sw0

  
  
  FUNCTION approx_ne0(a)
    TYPE(node) :: a
    REAL(DP) :: approx_ne0
 
    REAL(DP) :: px, py

    px = a%x + a%h_value*.5_DP
    py = a%y + a%h_value*.5_DP

    approx_ne0 = interpolate3_2D_initial(a,px,py)
  ENDFUNCTION approx_ne0

  
  
  FUNCTION interpolate3(p1,p2,p3, data1,data2,data3, p)

    REAL(DP) :: interpolate3
    REAL(DP) :: p1,p2, p3  !3 points to interpolate
    REAL(DP) :: data1, data2, data3 !function values at p1, p2, p3
    REAL(DP) :: p  !point at wich approximation is happening

    REAL(DP) :: temp1, temp2, temp3 , dx

    dx = p2-p1

    temp1 = (p-p2)*(p-p3)*data1
    temp2 = (p-p1)*(p-p3)*data2
    temp3 = (p-p1)*(p-p2)*data3

    interpolate3 = (temp1 - 2*temp2 + temp3)/(2*dx**2)

  ENDFUNCTION interpolate3
  



  


  
  FUNCTION interpolate3_2D_initial(a, px, py)
    TYPE(node) :: a
    REAL(DP) :: px, py            !approximation point
    REAL(DP) :: interpolate3_2D_initial
    
    REAL(DP) :: px1, px2, px3
    REAL(DP) :: py1, py2, py3
    
    REAL(DP) :: datax11, datax21, datax31
    REAL(DP) :: datax12, datax22, datax32
    REAL(DP) :: datax13, datax23, datax33
    REAL(DP) :: fx1, fx2, fx3
    
    px1 = a%x - a%h_value ;    py1 = a%y - a%h_value
    px2 = a%x             ;    py2 = a%y
    px3 = a%x + a%h_value ;    py3 = a%y + a%h_value
    
    
    datax11 = initial_data(px1,py1)
    datax21 = initial_data(px2,py1)
    datax31 = initial_data(px3,py1)

    datax12 = initial_data(px1,py2)
    datax22 = initial_data(px2,py2)
    datax32 = initial_data(px3,py2)

    datax13 = initial_data(px1,py3)
    datax23 = initial_data(px2,py3)
    datax33 = initial_data(px3,py3)
    
    fx1 = interpolate3(px1,px2,px3, datax11,datax21,datax31, px)
    fx2 = interpolate3(px1,px2,px3, datax12,datax22,datax32, px)
    fx3 = interpolate3(px1,px2,px3, datax13,datax23,datax33, px)
    
    interpolate3_2D_initial = interpolate3(py1,py2,py3, fx1,fx2,fx3, py)
  ENDFUNCTION interpolate3_2D_initial



  
  RECURSIVE SUBROUTINE initial_check(a)
    TYPE(node) :: a
    

    !----------------------------------------------!
    ! INPUT : ROOT LATTICE - ONE NODE AT A TIME    !
    !                                              !
    ! COMPARES ACTUAL VALUE OF WOULD BE CHILDREN   !
    ! TO INTERPOLATED VALUES.  IF INTERP. VALUE IS !
    ! NOT GOOD ENOUGH, IT SPLITS THE NODE          !
    !----------------------------------------------!
    
    ! rmk: if any of the would be children nodes is not accurately approximated, 
    ! they are all created, so we check them one at a time, stopping at the first 
    ! point of insufficient accuracy.

    IF( ASSOCIATED(a%ne_child)) THEN !GO ALL THE WAY TO THE LEAVES
       CALL initial_check(a%nw_child)
       CALL initial_check(a%sw_child)
       CALL initial_check(a%se_child)
       CALL initial_check(a%ne_child)
    ELSEIF(a%h_value > h_min) THEN !IF NOT AT MAX LEVEL, PERFORM CHECK
       IF( ABS(initial_data(a%x , a%y + a%h_value*.5) & 
            - approx_nw0(a)) > eps ) THEN

          CALL split_node(a)
          CALL initial_check(a%nw_child)
          CALL initial_check(a%sw_child)
          CALL initial_check(a%se_child)
          CALL initial_check(a%ne_child)

       ELSEIF( ABS(initial_data(a%x + a%h_value*.5 , a%y ) &
            - approx_se0(a)) > eps)THEN

          CALL split_node(a)

          CALL initial_check(a%nw_child)
          CALL initial_check(a%sw_child)
          CALL initial_check(a%se_child)
          CALL initial_check(a%ne_child)

       ELSEIF( ABS(initial_data(a%x , a%y) - &
            approx_sw0(a)) > eps)THEN

          CALL split_node(a)

          CALL initial_check(a%nw_child)
          CALL initial_check(a%sw_child)
          CALL initial_check(a%se_child)
          CALL initial_check(a%ne_child)
       ELSEIF( ABS(initial_data(a%x + a%h_value*.5, a%y + a%h_value*.5) - &
            approx_ne0(a)) > eps)THEN

          CALL split_node(a)
          CALL initial_check(a%nw_child)
          CALL initial_check(a%sw_child)
          CALL initial_check(a%se_child)
          CALL initial_check(a%ne_child)
       ENDIF

    ENDIF
  ENDSUBROUTINE initial_check





  FUNCTION  initial_data(x,y)
    REAL(DP) :: x,y
    REAL(DP) :: initial_data
    REAL(DP) :: temp
    temp = -100.*(x**2 + y**2)/(4*1.)
    initial_data = 100.*exp(temp)/(4*PI_D*1.)
  ENDFUNCTION initial_data

  
  RECURSIVE SUBROUTINE write_fn_value(a,file_id)
    TYPE(node), INTENT(IN) :: a
    INTEGER :: file_id

    IF(ASSOCIATED(a%ne_child))THEN
       CALL write_fn_value(a%ne_child, file_id)
       CALL write_fn_value(a%nw_child, file_id)
       CALL write_fn_value(a%se_child, file_id)
       CALL write_fn_value(a%sw_child, file_id)
    ELSE
       WRITE(file_id, "(3(1x,F15.10))") a%x, a%y, a%fn_value 
    ENDIF
  ENDSUBROUTINE write_fn_value
ENDMODULE GENESIS
