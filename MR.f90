MODULE MR
  USE HTYPE
  USE GENESIS
  USE parameters
  USE INTERPOL


CONTAINS
  
  RECURSIVE SUBROUTINE update_leaves(a)
    TYPE(node), INTENT(INOUT) :: a
    
    IF(ASSOCIATED(a%ne_child))THEN
       CALL update_leaves(a%ne_child)
       CALL update_leaves(a%nw_child)
       CALL update_leaves(a%se_child)
       CALL update_leaves(a%sw_child)
    ELSE
       a%fn_value = a%fn_new
    ENDIF
  ENDSUBROUTINE update_leaves
  

  RECURSIVE SUBROUTINE details(a)
    TYPE(node), INTENT(INOUT) :: a
    REAL(DP) :: temp_nw, temp_ne, temp_sw, temp_se
    
    IF(ASSOCIATED(a%ne_child))THEN
       CALL approx_ne(a, temp_ne)
       a%ne_child%small_detail = abs(temp_ne-a%ne_child%fn_value ) <= eps
       a%ne_child%detail = abs(temp_ne - a%ne_child%fn_value)

       CALL approx_se(a, temp_se)
       a%se_child%detail = abs(temp_se - a%se_child%fn_value)
       a%se_child%small_detail = abs(temp_se - a%se_child%fn_value) <= eps


       CALL approx_sw(a, temp_sw)
       a%sw_child%detail = abs(temp_sw - a%sw_child%fn_value)
       a%sw_child%small_detail = abs(temp_sw - a%sw_child%fn_value) <= eps

       
       CALL approx_nw(a, temp_nw)
       a%nw_child%detail = abs(temp_nw - a%nw_child%fn_value)
       a%nw_child%small_detail = abs(temp_nw - a%nw_child%fn_value) <= eps
       
       
       ! MARK PARENTS OF SMALL-DETAILED CHILDREN AS DELETABLE
       a%ne_child%deletable = a%ne_child%small_detail .AND. a%nw_child%small_detail &
            .AND. a%sw_child%small_detail .AND. a%se_child%small_detail
       a%nw_child%deletable = a%ne_child%deletable
       a%sw_child%deletable = a%ne_child%deletable
       a%se_child%deletable = a%ne_child%deletable
       
       CALL details(a%ne_child)
       CALL details(a%nw_child)
       CALL details(a%se_child)
       CALL details(a%sw_child)

    ENDIF
    
  ENDSUBROUTINE details
  
  RECURSIVE SUBROUTINE find_deletable(a)
    TYPE(node), INTENT(INOUT) :: a
    
    IF(ASSOCIATED(a%ne_child))THEN
       CALL find_deletable(a%ne_child)
       CALL find_deletable(a%nw_child)
       CALL find_deletable(a%se_child)
       CALL find_deletable(a%sw_child)
       
       a%ne_child%deletable = a%ne_child%small_detail .AND. a%nw_child%small_detail &
            .AND. a%sw_child%small_detail .AND. a%se_child%small_detail
       
       a%nw_child%deletable = a%ne_child%deletable
       a%sw_child%deletable = a%ne_child%deletable
       a%se_child%deletable = a%ne_child%deletable
    ENDIF

  ENDSUBROUTINE find_deletable
  

  RECURSIVE SUBROUTINE reconstruct(a)
    TYPE(node), INTENT(INOUT) :: a
    
    IF(ASSOCIATED(a%ne_child))THEN
       CALL reconstruct(a%ne_child)
       CALL reconstruct(a%nw_child)
       CALL reconstruct(a%se_child)
       CALL reconstruct(a%sw_child)

    ELSEIF(.NOT. a%small_detail .AND. a%h_value > h_min) THEN
       CALL add_node(a)
    ENDIF

  ENDSUBROUTINE reconstruct


  RECURSIVE SUBROUTINE removal(a)
    TYPE(node), INTENT(INOUT) :: a
    
    LOGICAL :: no_virtual, no_e, no_w, no_n, no_s
    LOGICAL :: no_ne, no_nw, no_sw, no_se
    LOGICAL :: nodes_deletable, not_parents
    
    
    
    IF( ASSOCIATED(a%ne_child) ) THEN
       CALL removal(a%ne_child)       
       CALL removal(a%nw_child)
       CALL removal(a%se_child)
       CALL removal(a%sw_child)
       
       ! CHECK IF NODE AND IT'S CHILDREN ARE DELETABLE
       nodes_deletable = a%deletable .AND. a%ne_child%deletable .AND. & 
            a%nw_child%deletable .AND. a%se_child%deletable .AND. a%sw_child%deletable
       
       
       ! CHECK IF ANY OF THE CHILDREN ARE PARENTS
       not_parents = .NOT. ASSOCIATED(a%ne_child%ne_child) .AND. &
            .NOT. ASSOCIATED(a%nw_child%ne_child) .AND.  &
            .NOT. ASSOCIATED(a%se_child%ne_child) .AND.  &
            .NOT. ASSOCIATED(a%sw_child%ne_child)
       
       
       ! CHECK IF ANY OF THE CHILDREN NODES ARE UNCLES
       
       IF(.NOT. ASSOCIATED(a%nw_child%w_cous))THEN
          no_w = .TRUE.  ! THIS HAPPENS WHEN THERE ARENT ANY NODES TO THE WEST
       ELSE
          no_w = .NOT. ASSOCIATED(a%nw_child%w_cous%ne_child) .AND. &
               .NOT. ASSOCIATED(a%sw_child%w_cous%ne_child)
          
          IF(.NOT. ASSOCIATED(a%nw_child%w_cous%n_cous)) THEN
             no_nw = .TRUE.
          ELSE
             no_nw = .NOT. ASSOCIATED(a%nw_child%w_cous%n_cous%sw_child)
          ENDIF
          
       ENDIF
       
       IF(.NOT. ASSOCIATED(a%ne_child%e_cous)) THEN
          no_e = .TRUE.
       ELSE
          no_e = .NOT. ASSOCIATED(a%ne_child%e_cous%nw_child) .AND. &
               .NOT. ASSOCIATED(a%se_child%e_cous%nw_child)
       ENDIF
       
       
       IF(.NOT. ASSOCIATED(a%se_child%s_cous))THEN
          no_s = .TRUE.
       ELSE
          no_s = .NOT. ASSOCIATED(a%se_child%s_cous%ne_child) .AND. &
               .NOT. ASSOCIATED(a%sw_child%s_cous%ne_child)
       ENDIF
       
       
       IF(.NOT. ASSOCIATED(a%ne_child%n_cous))THEN
          no_n = .TRUE.
       ELSE
          no_n = .NOT. ASSOCIATED(a%ne_child%n_cous%se_child) .AND. &
               .NOT. ASSOCIATED(a%nw_child%n_cous%se_child)
       ENDIF
       
       
       !LOOK FOR NIECES AND NEPHEWS TO THE NORTH-WEST & SOUTH-WEST
       IF(.NOT. ASSOCIATED(a%nw_child%w_cous))THEN
          !no_nw
          no_nw = .TRUE.
          
          !no_sw
          no_sw = .TRUE.
       ELSE
          IF(.NOT. ASSOCIATED(a%nw_child%w_cous%n_cous)) THEN
             no_nw = .TRUE.
          ELSE
             no_nw = .NOT. ASSOCIATED(a%nw_child%w_cous%n_cous%sw_child)
          ENDIF
          
          IF(.NOT. ASSOCIATED(a%sw_child%w_cous%s_cous))THEN
             no_sw = .TRUE.
          ELSE
             no_sw = .NOT. ASSOCIATED(a%sw_child%w_cous%s_cous%nw_child)
          ENDIF
       ENDIF
       
       !LOOK FOR NIECES AND NEPHEWS TO THE NORTH-EAST & SOUTH-EAST
       IF(.NOT. ASSOCIATED(a%ne_child%e_cous))THEN
          !no_nw
          no_ne = .TRUE.
          
          !no_sw
          no_se = .TRUE.
       ELSE
          IF(.NOT. ASSOCIATED(a%ne_child%e_cous%n_cous)) THEN
             no_ne = .TRUE.
          ELSE
             no_ne = .NOT. ASSOCIATED(a%ne_child%e_cous%n_cous%se_child)
          ENDIF
          
          IF(.NOT. ASSOCIATED(a%se_child%e_cous%s_cous))THEN
             no_se = .TRUE.
          ELSE
             no_se = .NOT. ASSOCIATED(a%se_child%e_cous%s_cous%ne_child)
          ENDIF
       ENDIF

       
       
       no_virtual = no_w .AND. no_e .AND. no_n .AND. no_s &
            .AND. no_nw .AND. no_sw .AND. no_ne .AND. no_se
       
       ! CHECK IF THE NODE'S CHLDREN CAN BE REMOVED
       IF(no_virtual .AND. not_parents .AND. nodes_deletable)THEN
          ! PREPARE TO DELETE THE CHILD NODES

          ! CHECK IF ANY OF THE CHILDREN NODES ARE BEING 
          ! POINTED TO VIA ANY COUSINS.  IF SO, NULLIFY
          ! THOSE POINTERS

          !1. CHECK nw_child
          IF(ASSOCIATED(a%nw_child%n_cous))THEN
             NULLIFY(a%nw_child%n_cous%s_cous)
          ENDIF

          IF(ASSOCIATED(a%nw_child%w_cous))THEN
             NULLIFY(a%nw_child%w_cous%e_cous)
          ENDIF
          
          !2. CHECK ne_child
          IF(ASSOCIATED(a%ne_child%n_cous))THEN
             NULLIFY(a%ne_child%n_cous%s_cous)
          ENDIF
          IF(ASSOCIATED(a%ne_child%e_cous))THEN
             NULLIFY(a%ne_child%e_cous%w_cous)
          ENDIF

          !3. CHECK se_child
          IF(ASSOCIATED(a%se_child%s_cous))THEN
             NULLIFY(a%se_child%s_cous%n_cous)
          ENDIF
          IF(ASSOCIATED(a%se_child%e_cous))THEN
             NULLIFY(a%se_child%e_cous%w_cous)
          ENDIF

          !4. CHECK sw_child
          IF(ASSOCIATED(a%sw_child%s_cous))THEN
             NULLIFY(a%sw_child%s_cous%n_cous)
          ENDIF
          IF(ASSOCIATED(a%sw_child%w_cous))THEN
             NULLIFY(a%sw_child%w_cous%e_cous)
          ENDIF
          
          
          ! DEALLOCATE THE CHILDREN NODES
          DEALLOCATE(a%ne_child)
          DEALLOCATE(a%nw_child)
          DEALLOCATE(a%se_child)
          DEALLOCATE(a%sw_child)
          
          
        ENDIF
    ENDIF
    
  ENDSUBROUTINE removal
  !---------------------------------------------------------------------------!    
  
  RECURSIVE SUBROUTINE add_node(a)
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
    x_east = a%x + .5*a%h_value

    y_south = a%y 
    y_north = a%y +.5*a%h_value

    
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
    ! 5 - CHECK FOR UNCLES                      !
    !   - THIS WILL DEPEND ON THE INTERP. USED  !
    !   - FOR NOW, WE USE LINEAR INTERP.        !
    !-------------------------------------------!    
    
    ! NEED TO MAKE SURE a%w_cous AND a%s_cous
    ! ARE AVAILABLE
    IF( .NOT. ASSOCIATED(a%w_cous) ) THEN
       CALL add_node(a%parent%w_cous)
    ENDIF
    
    IF( .NOT. ASSOCIATED(a%s_cous)  ) THEN
       CALL add_node(a%parent%s_cous)
    ENDIF
    
    IF( .NOT. ASSOCIATED(a%e_cous)  )THEN
       CALL add_node(a%parent%e_cous)
    ENDIF
    
    IF( .NOT. ASSOCIATED(a%n_cous)  )THEN
       CALL add_node(a%parent%n_cous)
    ENDIF
    
    !nw_cous
    IF( .NOT. ASSOCIATED(a%w_cous%n_cous)  ) THEN
       CALL add_node(a%parent%w_cous%n_cous)
    ENDIF

    !ne_cous
    IF( .NOT. ASSOCIATED(a%e_cous%n_cous)  ) THEN
       CALL add_node(a%parent%e_cous%n_cous)
    ENDIF
    
    
    !sw_cous
    IF( .NOT. ASSOCIATED(a%w_cous%s_cous)  ) THEN
       CALL add_node(a%parent%w_cous%s_cous)
    ENDIF

    !se_cous
    IF( .NOT. ASSOCIATED(a%e_cous%s_cous)  ) THEN
       CALL add_node(a%parent%e_cous%s_cous)
    ENDIF    

    !-------------------------------------------!
    ! 2 - SET ALL OF THE APPROPRIATE VALUES     !
    !-------------------------------------------!
    
    ! 2.1 - a%nw_child
    a%nw_child%x = x_west
    a%nw_child%y = y_north

    a%nw_child%deletable = .true.
    a%nw_child%small_detail = .true.


    CALL approx_nw(a, a%nw_child%fn_value)
    a%nw_child%h_value = .5_DP*a%h_value


    ! 2.2 - a%sw_child
    a%sw_child%x = x_west
    a%sw_child%y = y_south
    

    CALL approx_sw(a, a%sw_child%fn_value)
    ! SINCE a%sw_child CORRESPONDS TO THE PARENT, THERE IS NO NEED TO INTERPOLATE.
!    a%sw_child%fn_value = a%fn_value
!    a%sw_child%stress_tensor = 0.0
    a%sw_child%h_value = .5_DP*a%h_value

    a%sw_child%deletable = .true.
    a%sw_child%small_detail = .true.



    ! 2.3 - a%ne_child
    a%ne_child%x = x_east
    a%ne_child%y = y_north

    CALL approx_ne(a, a%ne_child%fn_value)
    a%ne_child%h_value = .5_DP*a%h_value

    a%ne_child%deletable = .true.
    a%ne_child%small_detail = .true.

    ! 2.4 - a%se_child
    a%se_child%x = x_east
    a%se_child%y = y_south

    CALL approx_se(a, a%se_child%fn_value)
    a%se_child%h_value = .5_DP*a%h_value

    a%se_child%deletable = .true.
    a%se_child%small_detail = .true.


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

  ENDSUBROUTINE add_node

  RECURSIVE SUBROUTINE scalar_projection(a)
    
    TYPE (node), INTENT(INOUT) :: a
    
    IF( ASSOCIATED(a%ne_child)) THEN

       CALL scalar_projection(a%ne_child)
       CALL scalar_projection(a%nw_child)
       CALL scalar_projection(a%se_child)
       CALL scalar_projection(a%sw_child)
       a%fn_value = a%sw_child%fn_value
    ENDIF
  ENDSUBROUTINE scalar_projection

ENDMODULE MR
