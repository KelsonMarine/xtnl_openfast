!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2015-2016  National Renewable Energy Laboratory
!
!    This file is part of ExtLoads.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
!**********************************************************************************************************************************
! File last committed: $Date$
! (File) Revision #: $Rev$
! URL: $HeadURL$
!**********************************************************************************************************************************
!> ExtPtfmLoads is a time-domain loads module for horizontal-axis wind turbines.
module ExtPtfmLoads

   use NWTC_Library
   use ExtPtfmLoads_Types
   use InflowWind_IO_Types
   use InflowWind_IO

   implicit none

   private

   ! ..... Public Subroutines ...................................................................................................

   public :: ExtPtfmLd_Init                           ! Initialization routine
   public :: ExtPtfmLd_End                            ! Ending routine (includes clean up)
   public :: ExtPtfmLd_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                                     !   continuous states, and updating discrete states
   public :: ExtPtfmLd_CalcOutput                     ! Routine for computing outputs
   public :: ExtPtfmLd_ConvertOpDataForOpenFAST        ! Routine to convert Output data for OpenFAST
   public :: ExtPtfmLd_ConvertInpDataForExtProg        ! Routine to convert Input data for external programs

contains    

!> Helper functions for the module

!> This routine sets the error status and error message for a routine, it's a simplified version of SetErrStat from NWTC_Library
subroutine SetErrStatSimple(ErrStat, ErrMess, RoutineName, LineNumber)
  INTEGER(IntKi), INTENT(INOUT)        :: ErrStat      ! Error status of the operation
  CHARACTER(*),   INTENT(INOUT)        :: ErrMess      ! Error message if ErrStat /= ErrID_None
  CHARACTER(*),   INTENT(IN   )        :: RoutineName  ! Name of the routine error occurred in
  INTEGER(IntKi), INTENT(IN), OPTIONAL :: LineNumber   ! Line of input file 
  if (ErrStat /= ErrID_None) then
     write(ErrMess,'(A)') TRIM(RoutineName)//':'//TRIM(ErrMess)
     if (present(LineNumber)) then
         ErrMess = TRIM(ErrMess)//' Line: '//TRIM(Num2LStr(LineNumber))//'.'
     endif
  end if
end subroutine SetErrStatSimple
!----------------------------------------------------------------------------------------------------------------------------------   
!> This subroutine sets the initialization output data structure, which contains data to be returned to the calling program (e.g.,
!! FAST)   
subroutine ExtPtfmLd_SetInitOut(p, InitOut, errStat, errMsg)

   type(ExtPtfmLd_InitOutputType),    intent(inout)  :: InitOut          ! output data
   type(ExtPtfmLd_ParameterType),     intent(in   )  :: p                ! Parameters
   integer(IntKi),                intent(  out)  :: errStat          ! Error status of the operation
   character(*),                  intent(  out)  :: errMsg           ! Error message if ErrStat /= ErrID_None


      ! Local variables
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'ExtPtfmLd_SetInitOut'
   
   
   
   integer(IntKi)                               :: i, j, k, f
   integer(IntKi)                               :: NumCoords
#ifdef DBG_OUTS
   integer(IntKi)                               :: m
   character(5)                                 ::chanPrefix
#endif   
      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""
   
end subroutine ExtPtfmLd_SetInitOut

!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine is called at the start of the simulation to perform initialization steps.
!! The parameters are set here and not changed during the simulation.
!! The initial states and initial guess for the input are defined.
SUBROUTINE ExtPtfmLd_Init( InitInp, u, p, x, xd, z, OtherState, y, m, Interval, InitOut, ErrStat, ErrMsg )
!..................................................................................................................................

   TYPE(ExtPtfmLd_InitInputType),       INTENT(IN   )  :: InitInp     !< Input data for initialization routine
   TYPE(ExtPtfmLd_InputType),           INTENT(  OUT)  :: u           !< An initial guess for the input; input mesh must be defined
   TYPE(ExtPtfmLd_ParameterType),       INTENT(  OUT)  :: p           !< Parameters
   TYPE(ExtPtfmLd_ContinuousStateType), INTENT(  OUT)  :: x           !< Initial continuous states
   TYPE(ExtPtfmLd_DiscreteStateType),   INTENT(  OUT)  :: xd          !< Initial discrete states
   TYPE(ExtPtfmLd_ConstraintStateType), INTENT(  OUT)  :: z           !< Initial guess of the constraint states
   TYPE(ExtPtfmLd_OtherStateType),      INTENT(  OUT)  :: OtherState  !< Initial other states (logical, etc)
   TYPE(ExtPtfmLd_OutputType),          INTENT(  OUT)  :: y           !< Initial system outputs (outputs are not calculated;
                                                                     !!   only the output mesh is initialized)
   TYPE(ExtPtfmLd_MiscVarType),         INTENT(  OUT)  :: m           !< Misc variables for optimization (not copied in glue code)
   REAL(DbKi),                        INTENT(INOUT)  :: Interval    !< Coupling interval in seconds: the rate that
                                                                     !!   (1) ExtPtfmLd_UpdateStates() is called in loose coupling &
                                                                     !!   (2) ExtPtfmLd_UpdateDiscState() is called in tight coupling.
                                                                     !!   Input is the suggested time from the glue code;
                                                                     !!   Output is the actual coupling interval that will be used
                                                                     !!   by the glue code.
   TYPE(ExtPtfmLd_InitOutputType),      INTENT(  OUT)  :: InitOut     !< Output for initialization routine
   INTEGER(IntKi),                    INTENT(  OUT)  :: ErrStat     !< Error status of the operation
   CHARACTER(*),                      INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None

      ! Local variables
   integer(IntKi)                              :: i             ! loop counter
   type(Points_InitInputType)                  :: Points_InitInput
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
      
   character(*), parameter                     :: RoutineName = 'ExtPtfmLd_Init'
   
   
      ! Initialize variables for this routine

   errStat = ErrID_None
   errMsg  = ""

   call NWTC_Init( )

      ! Initialize the NWTC Subroutine Library

   call Init_meshes(u, y, InitInp, ErrStat2, ErrMsg2); if(Failed()) return


   CALL AllocPAry( u%DX_u%ptfmDef, 3+9+3+3+3+3, 'ptfmDef', ErrStat2, ErrMsg2 ); if(Failed()) return
   ! allocate( u%DX_u%ptfmDef(3+9+3+3), stat=errStat )
   ! if (errStat /= 0) then
   !    call SetErrStat( ErrID_Fatal, 'Error allocating u%DX_u%ptfmDef.', ErrStat, ErrMsg, RoutineName )      
   !    return
   ! end if

   u%DX_u%C_obj%ptfmDef_Len = 3+9+3+3+3+3
   u%DX_u%C_obj%ptfmDef = C_LOC(u%DX_u%ptfmDef(1))

   CALL AllocPAry( y%DX_y%ptfmLd, 6, 'ptfmLd', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   y%DX_y%c_obj%ptfmLd_Len = 6; y%DX_y%c_obj%ptfmLd = C_LOC( y%DX_y%ptfmLd(1) )

      ! Set parameters here
   ! p%NumBlds = InitInp%NumBlades
   ! call AllocAry(p%NumBldNds, p%NumBlds, 'NumBldNds', ErrStat2,ErrMsg2)
   ! call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
   !   if (ErrStat >= AbortErrLev) return
   ! p%NumBldNds(:) = InitInp%NumBldNodes(:)
   ! p%nTotBldNds = sum(p%NumBldNds(:))
   ! p%NumTwrNds = InitInp%NumTwrNds
   ! p%TwrAero = .true.

   ! p%az_blend_mean = InitInp%az_blend_mean
   ! p%az_blend_delta = InitInp%az_blend_delta
   
      !............................................................................................
      ! Define and initialize inputs here 
      !............................................................................................

   write(*,*) 'Initializing U '
   
   ! call Init_u( u, p, InitInp, errStat2, errMsg2 ) 
   !    call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
   !    if (ErrStat >= AbortErrLev) return


  ! Initialize discrete states
   ! m%az = 0.0 
   ! m%phi_cfd = 0.0

   ! write(*,*) 'Initializing y '

      !............................................................................................
      ! Define outputs here
      !............................................................................................
   ! call Init_y(y, u, m, p, errStat2, errMsg2) ! do this after input meshes have been initialized
   !    call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
   !    if (ErrStat >= AbortErrLev) return
   
      
      !............................................................................................
      ! Define initialization output here
      !............................................................................................
   call ExtPtfmLd_SetInitOut(p, InitOut, errStat2, errMsg2)
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
   

contains
   logical function Failed()
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      Failed = ErrStat >= AbortErrLev
   end function Failed
 
end subroutine ExtPtfmLd_Init
!----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE Init_meshes(u, y, InitInp, ErrStat, ErrMsg)
   TYPE(ExtPtfmLd_InputType),           INTENT(INOUT)  :: u           !< System inputs
   TYPE(ExtPtfmLd_OutputType),          INTENT(INOUT)  :: y           !< System outputs
   TYPE(ExtPtfmLd_InitInputType),       INTENT(IN   )  :: InitInp     !< Input data for initialization routine
   INTEGER(IntKi),                    INTENT(  OUT)  :: ErrStat     !< Error status of the operation
   CHARACTER(*),                      INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None
   ! Create the input and output meshes associated with platform loads
   CALL MeshCreate(  BlankMesh         = u%PtfmMotion     , &
                     IOS               = COMPONENT_INPUT  , &
                     Nnodes            = 1                , &
                     ErrStat           = ErrStat          , &
                     ErrMess           = ErrMsg           , &
                     TranslationDisp   = .TRUE.           , &
                     Orientation       = .TRUE.           , &
                     TranslationVel    = .TRUE.           , &
                     RotationVel       = .TRUE.           , &
                     TranslationAcc    = .TRUE.           , &
                     RotationAcc       = .TRUE.)
   if(Failed()) return
      
   ! Create the node on the mesh, the node is located at the PlatformRefzt, to match ElastoDyn
   CALL MeshPositionNode (u%PtfmMotion, 1, (/0.0_ReKi, 0.0_ReKi, InitInp%PtfmRefzt/), ErrStat, ErrMsg ); if(Failed()) return
   ! Create the mesh element
   CALL MeshConstructElement (  u%PtfmMotion, ELEMENT_POINT, ErrStat, ErrMsg, 1 ); if(Failed()) return
   CALL MeshCommit ( u%PtfmMotion, ErrStat, ErrMsg ); if(Failed()) return
   ! the output mesh is a sibling of the input:
   CALL MeshCopy( SrcMesh=u%PtfmMotion, DestMesh=y%PtfmMesh, CtrlCode=MESH_SIBLING, IOS=COMPONENT_OUTPUT, &
                  ErrStat=ErrStat, ErrMess=ErrMsg, Force=.TRUE., Moment=.TRUE. )
   if(Failed()) return
CONTAINS
    logical function Failed()
        CALL SetErrStatSimple(ErrStat, ErrMsg, 'Init_meshes')
        Failed =  ErrStat >= AbortErrLev
    end function Failed
END SUBROUTINE Init_meshes
!----------------------------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine initializes ExtPtfmLoads meshes and output array variables for use during the simulation.
! subroutine Init_y(y, u, m, p, errStat, errMsg)
!    type(ExtPtfmLd_OutputType),           intent(  out)  :: y               !< Module outputs
!    type(ExtPtfmLd_InputType),            intent(inout)  :: u               !< Module inputs -- intent(out) because of mesh sibling copy
!    type(ExtPtfmLd_MiscVarType),          intent(inout)  :: m               !< Module misc var
!    type(ExtPtfmLd_ParameterType),        intent(in   )  :: p               !< Parameters
!    integer(IntKi),                intent(  out)  :: errStat         !< Error status of the operation
!    character(*),                  intent(  out)  :: errMsg          !< Error message if ErrStat /= ErrID_None


!       ! Local variables
!    integer(intKi)                               :: k                 ! loop counter for blades
!    integer(intKi)                               :: ErrStat2          ! temporary Error status
!    character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
!    character(*), parameter                      :: RoutineName = 'Init_y'

!       ! Initialize variables for this routine

!    errStat = ErrID_None
!    errMsg  = ""

!    if (p%TwrAero) then

!       call MeshCopy ( SrcMesh  = u%TowerMotion    &
!            , DestMesh = y%TowerLoad      &
!            , CtrlCode = MESH_SIBLING     &
!            , IOS      = COMPONENT_OUTPUT &
!            , force    = .TRUE.           &
!            , moment   = .TRUE.           &
!            , ErrStat  = ErrStat2         &
!            , ErrMess  = ErrMsg2          )

!       call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
!       if (ErrStat >= AbortErrLev) RETURN         

!       call MeshCopy ( SrcMesh  = u%TowerMotion    &
!            , DestMesh = y%TowerLoadAD      &
!            , CtrlCode = MESH_COUSIN     &
!            , IOS      = COMPONENT_OUTPUT &
!            , force    = .TRUE.           &
!            , moment   = .TRUE.           &
!            , ErrStat  = ErrStat2         &
!            , ErrMess  = ErrMsg2          )

!       call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName ) 
!       if (ErrStat >= AbortErrLev) RETURN

!       !call MeshCommit(y%TowerLoadAD, errStat2, errMsg2 )
!       !call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      
!       !y%TowerLoad%force = 0.0_ReKi  ! shouldn't have to initialize this
!       !y%TowerLoad%moment= 0.0_ReKi  ! shouldn't have to initialize this
!    else
!       y%TowerLoad%nnodes = 0
!       y%TowerLoadAD%nnodes = 0
!    end if

!    allocate( y%BladeLoad(p%NumBlds), stat=ErrStat2 )
!    if (errStat2 /= 0) then
!       call SetErrStat( ErrID_Fatal, 'Error allocating y%BladeLoad.', ErrStat, ErrMsg, RoutineName )      
!       return
!    end if

!    allocate( y%BladeLoadAD(p%NumBlds), stat=ErrStat2 )
!    if (errStat2 /= 0) then
!       call SetErrStat( ErrID_Fatal, 'Error allocating y%BladeLoad.', ErrStat, ErrMsg, RoutineName )      
!       return
!    end if
   
!    do k = 1, p%NumBlds

!       call MeshCopy ( SrcMesh  = u%BladeMotion(k) &
!            , DestMesh = y%BladeLoad(k)   &
!            , CtrlCode = MESH_SIBLING     &
!            , IOS      = COMPONENT_OUTPUT &
!            , force    = .TRUE.           &
!            , moment   = .TRUE.           &
!            , ErrStat  = ErrStat2         &
!            , ErrMess  = ErrMsg2          )

!       call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

!       call MeshCopy ( SrcMesh  = u%BladeMotion(k) &
!            , DestMesh = y%BladeLoadAD(k)   &
!            , CtrlCode = MESH_COUSIN     &
!            , IOS      = COMPONENT_OUTPUT &
!            , force    = .TRUE.           &
!            , moment   = .TRUE.           &
!            , ErrStat  = ErrStat2         &
!            , ErrMess  = ErrMsg2          )

!       call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

!       !call MeshCommit(y%BladeLoadAD(k), errStat2, errMsg2 )
!       !call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
      

!    end do

!    CALL AllocPAry( y%DX_y%twrLd, p%NumTwrNds*6, 'twrLd', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( y%DX_y%bldLd, p%nTotBldNds*6, 'bldLd', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

!    ! make sure the C versions are synced with these arrays
!    y%DX_y%c_obj%twrLd_Len = p%NumTwrNds*6; y%DX_y%c_obj%twrLd = C_LOC( y%DX_y%twrLd(1) )
!    y%DX_y%c_obj%bldLd_Len = p%nTotBldNds*6; y%DX_y%c_obj%bldLd = C_LOC( y%DX_y%bldLd(1) )

!    call ExtPtfmLd_ConvertOpDataForOpenFAST(y, u, m, p, ErrStat2, ErrMsg2 )
!    call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )   
   
! end subroutine Init_y
! !----------------------------------------------------------------------------------------------------------------------------------
! !> This routine initializes ExtPtfmLoads meshes and input array variables for use during the simulation.
! subroutine Init_u( u, p, InitInp, errStat, errMsg )
! !..................................................................................................................................

!   USE BeamDyn_IO, ONLY: BD_CrvExtractCrv
  
!    type(ExtPtfmLd_InputType),           intent(  out)  :: u                 !< Input data
!    type(ExtPtfmLd_ParameterType),       intent(inout)  :: p                 !< Parameters (inout so can update DX_p)
!    type(ExtPtfmLd_InitInputType),       intent(in   )  :: InitInp           !< Input data for ExtPtfmLd initialization routine
!    integer(IntKi),               intent(  out)  :: errStat           !< Error status of the operation
!    character(*),                 intent(  out)  :: errMsg            !< Error message if ErrStat /= ErrID_None


!       ! Local variables
!    real(reKi)                                   :: position(3)       ! node reference position
!    real(reKi)                                   :: positionL(3)      ! node local position
!    real(R8Ki)                                   :: theta(3)          ! Euler angles
!    real(R8Ki)                                   :: orientation(3,3)  ! node reference orientation
!    real(R8Ki)                                   :: orientationL(3,3) ! node local orientation
   
!    real(R8Ki)                                   :: wm_crv(3)         ! Wiener-Milenkovic parameters
!    integer(IntKi)                               :: j                 ! counter for nodes
!    integer(IntKi)                               :: jTot              ! counter for blade nodes
!    integer(IntKi)                               :: k                 ! counter for blades

!    integer(intKi)                               :: ErrStat2          ! temporary Error status
!    character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
!    character(*), parameter                      :: RoutineName = 'Init_u'

!       ! Initialize variables for this routine

!    ErrStat = ErrID_None
!    ErrMsg  = ""


!    u%az = 0.0
!       ! Meshes for motion inputs (ElastoDyn and/or BeamDyn)
!          !................
!          ! tower
!          !................
!    if (p%NumTwrNds > 0) then
      
!       call MeshCreate ( BlankMesh = u%TowerMotion   &
!                        ,IOS       = COMPONENT_INPUT &
!                        ,Nnodes    = p%NumTwrNds     &
!                        ,ErrStat   = ErrStat2        &
!                        ,ErrMess   = ErrMsg2         &
!                        ,Orientation     = .true.    &
!                        ,TranslationDisp = .true.    &
!                        ,TranslationVel  = .true.    &
!                        ,RotationVel = .true.        &
!                       )
!             call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

!       if (errStat >= AbortErrLev) return
            
!          ! set node initial position/orientation
!       position = 0.0_ReKi
!       do j=1,p%NumTwrNds         
!          position(:) = InitInp%TwrPos(:,j)
         
!          call MeshPositionNode(u%TowerMotion, j, position, errStat2, errMsg2)  ! orientation is identity by default
!             call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
!       end do !j
         
!          ! create point elements
!       do j=1,p%NumTwrNds
!          call MeshConstructElement( u%TowerMotion, ELEMENT_POINT, errStat2, errMsg2, p1=j )
!             call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
!       end do !j
            
!       call MeshCommit(u%TowerMotion, errStat2, errMsg2 )
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
!       if (errStat >= AbortErrLev) return

      
!       u%TowerMotion%Orientation     = u%TowerMotion%RefOrientation
!       u%TowerMotion%TranslationDisp = 0.0_R8Ki
!       u%TowerMotion%TranslationVel  = 0.0_ReKi
!       u%TowerMotion%RotationVel = 0.0_ReKi
      
!    end if ! we compute tower loads
   
!          !................
!          ! hub
!          !................
   
!       call MeshCreate ( BlankMesh = u%HubMotion     &
!                        ,IOS       = COMPONENT_INPUT &
!                        ,Nnodes    = 1               &
!                        ,ErrStat   = ErrStat2        &
!                        ,ErrMess   = ErrMsg2         &
!                        ,Orientation     = .true.    &
!                        ,TranslationDisp = .true.    &
!                        ,TranslationVel  = .true.    &
!                        ,RotationVel     = .true.    &
!                       )
!             call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

!       if (errStat >= AbortErrLev) return
                     
!       call MeshPositionNode(u%HubMotion, 1, InitInp%HubPos, errStat2, errMsg2, InitInp%HubOrient)
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         
!       call MeshConstructElement( u%HubMotion, ELEMENT_POINT, errStat2, errMsg2, p1=1 )
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
!       call MeshCommit(u%HubMotion, errStat2, errMsg2 )
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
!       if (errStat >= AbortErrLev) return

         
!       u%HubMotion%Orientation     = u%HubMotion%RefOrientation
!       u%HubMotion%TranslationDisp = 0.0_R8Ki
!       u%HubMotion%TranslationVel = 0.0_R8Ki
!       u%HubMotion%RotationVel     = 0.0_R8Ki   

!          !................
!          ! nacelle
!          !................
   
!       call MeshCreate ( BlankMesh = u%NacelleMotion     &
!                        ,IOS       = COMPONENT_INPUT &
!                        ,Nnodes    = 1               &
!                        ,ErrStat   = ErrStat2        &
!                        ,ErrMess   = ErrMsg2         &
!                        ,Orientation     = .true.    &
!                        ,TranslationDisp = .true.    &
!                        ,TranslationVel  = .true.    &
!                        ,RotationVel     = .true.    &
!                       )
!             call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

!       if (errStat >= AbortErrLev) return
                     
!       call MeshPositionNode(u%NacelleMotion, 1, InitInp%NacellePos, errStat2, errMsg2, InitInp%NacelleOrient)
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         
!       call MeshConstructElement( u%NacelleMotion, ELEMENT_POINT, errStat2, errMsg2, p1=1 )
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
!       call MeshCommit(u%NacelleMotion, errStat2, errMsg2 )
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
!       if (errStat >= AbortErrLev) return

         
!       u%NacelleMotion%Orientation     = u%NacelleMotion%RefOrientation
!       u%NacelleMotion%TranslationDisp = 0.0_R8Ki
!       u%NacelleMotion%TranslationVel = 0.0_R8Ki
!       u%NacelleMotion%RotationVel     = 0.0_R8Ki   
      
!          !................
!          ! blades
!          !................

!       allocate( u%BladeRootMotion(p%NumBlds), STAT = ErrStat2 )
!       if (ErrStat2 /= 0) then
!          call SetErrStat( ErrID_Fatal, 'Error allocating u%BladeRootMotion array.', ErrStat, ErrMsg, RoutineName )
!          return
!       end if
      
!       allocate( u%BladeMotion(p%NumBlds), STAT = ErrStat2 )
!       if (ErrStat2 /= 0) then
!          call SetErrStat( ErrID_Fatal, 'Error allocating u%BladeMotion array.', ErrStat, ErrMsg, RoutineName )
!          return
!       end if

!       do k=1,p%NumBlds

!          call MeshCreate ( BlankMesh = u%BladeRootMotion(k)     &
!               ,IOS       = COMPONENT_INPUT &
!               ,Nnodes    = 1               &
!               ,ErrStat   = ErrStat2        &
!               ,ErrMess   = ErrMsg2         &
!               ,Orientation     = .true.    &
!               ,TranslationDisp = .true.    &
!               ,TranslationVel  = .true.    &
!               ,RotationVel     = .true.    &
!               )
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         
!          if (errStat >= AbortErrLev) return
         
!          call MeshPositionNode(u%BladeRootMotion(k), 1, InitInp%BldRootPos(:,k), errStat2, errMsg2, InitInp%BldRootOrient(:,:,k))
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         
!          call MeshConstructElement( u%BladeRootMotion(k), ELEMENT_POINT, errStat2, errMsg2, p1=1 )
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         
!          call MeshCommit(u%BladeRootMotion(k), errStat2, errMsg2 )
!          call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
         
!          if (errStat >= AbortErrLev) return
         
!          u%BladeRootMotion(k)%Orientation     = u%BladeRootMotion(k)%RefOrientation
!          u%BladeRootMotion(k)%TranslationDisp = 0.0_R8Ki
!          u%BladeRootMotion(k)%TranslationVel  = 0.0_R8Ki
!          u%BladeRootMotion(k)%RotationVel     = 0.0_R8Ki   
         
!          call MeshCreate ( BlankMesh = u%BladeMotion(k)                     &
!                           ,IOS       = COMPONENT_INPUT                      &
!                           ,Nnodes    = InitInp%NumBldNodes(k) &
!                           ,ErrStat   = ErrStat2                             &
!                           ,ErrMess   = ErrMsg2                              &
!                           ,Orientation     = .true.                         &
!                           ,TranslationDisp = .true.                         &
!                           ,TranslationVel  = .true.                         &
!                           ,RotationVel = .true.                             &
!                          )
!                call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )

!          if (errStat >= AbortErrLev) return
            
                        
!          do j=1,InitInp%NumBldNodes(k)

!                ! reference position of the jth node in the kth blade:
!             position(:) = InitInp%BldPos(:,j,k)
                                 
!                ! reference orientation of the jth node in the kth blade
!             orientation(:,:) = InitInp%BldOrient(:,:,j,k)

            
!             call MeshPositionNode(u%BladeMotion(k), j, position, errStat2, errMsg2, orientation)
!                call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
               
!          end do ! j=blade nodes
         
!             ! create point elements
!          do j=1,InitInp%NumBldNodes(k)
!             call MeshConstructElement( u%BladeMotion(k), ELEMENT_POINT, errStat2, errMsg2, p1=j )
!                call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
!          end do !j
            
!          call MeshCommit(u%BladeMotion(k), errStat2, errMsg2 )
!             call SetErrStat( errStat2, errMsg2, errStat, errMsg, RoutineName )
            
!          if (errStat >= AbortErrLev) return
      
!          u%BladeMotion(k)%Orientation     = u%BladeMotion(k)%RefOrientation
!          u%BladeMotion(k)%TranslationDisp = 0.0_R8Ki
!          u%BladeMotion(k)%TranslationVel  = 0.0_R8Ki
!          u%BladeMotion(k)%RotationVel = 0.0_R8Ki
   
!    end do !k=numBlades

!    ! Set the parameters first
!    CALL AllocPAry( p%DX_p%nTowerNodes, 1, 'nTowerNodes', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    p%DX_p%c_obj%nTowerNodes_Len = 1; p%DX_p%c_obj%nTowerNodes = C_LOC( p%DX_p%nTowerNodes(1) )
!    p%DX_p%nTowerNodes(1) = p%NumTwrNds
!    CALL AllocPAry( p%DX_p%nBlades, 1, 'nBlades', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    p%DX_p%c_obj%nBlades_Len = 1; p%DX_p%c_obj%nBlades = C_LOC( p%DX_p%nBlades(1) )
!    p%DX_p%nBlades(1) = p%NumBlds
!    CALL AllocPAry( p%DX_p%nBladeNodes, p%NumBlds, 'nBladeNodes', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    p%DX_p%c_obj%nBladeNodes_Len = p%NumBlds; p%DX_p%c_obj%nBladeNodes = C_LOC( p%DX_p%nBladeNodes(1) )
!    p%DX_p%nBladeNodes(:) = p%NumBldNds(:)

!    ! Set the reference positions next
!    CALL AllocPAry( p%DX_p%twrRefPos, p%NumTwrNds*6, 'twrRefPos', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( p%DX_p%bldRefPos, p%nTotBldNds*6, 'bldRefPos', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( p%DX_p%hubRefPos, 6, 'hubRefPos', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( p%DX_p%nacRefPos, 6, 'nacRefPos', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry (p%DX_p%bldRootRefPos, p%NumBlds*6, 'bldRootRefPos', ErrStat2, ErrMsg2); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

!    ! make sure the C versions are synced with these arrays
!    p%DX_p%c_obj%twrRefPos_Len = p%NumTwrNds*6; p%DX_p%c_obj%twrRefPos = C_LOC( p%DX_p%twrRefPos(1) )
!    p%DX_p%c_obj%bldRefPos_Len = p%nTotBldNds*6; p%DX_p%c_obj%bldRefPos = C_LOC( p%DX_p%bldRefPos(1) )
!    p%DX_p%c_obj%hubRefPos_Len = 6; p%DX_p%c_obj%hubRefPos = C_LOC( p%DX_p%hubRefPos(1) )
!    p%DX_p%c_obj%nacRefPos_Len = 6; p%DX_p%c_obj%nacRefPos = C_LOC( p%DX_p%nacRefPos(1) )
!    p%DX_p%c_obj%bldRootRefPos_Len = p%NumBlds*6; p%DX_p%c_obj%bldRootRefPos = C_LOC( p%DX_p%bldRootRefPos(1) )
   
!    if (p%TwrAero) then
!       do j=1,p%NumTwrNds
!          call BD_CrvExtractCrv(u%TowerMotion%RefOrientation(:,:,j), wm_crv, ErrStat2, ErrMsg2)
!          call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

!          p%DX_p%twrRefPos((j-1)*6+1:(j-1)*6+3) = u%TowerMotion%Position(:,j)
!          p%DX_p%twrRefPos((j-1)*6+4:(j-1)*6+6) = wm_crv
!       end do
!    end if

!    jTot = 1
!    do k=1,p%NumBlds
!       do j=1,p%NumBldNds(k)
!          call BD_CrvExtractCrv(u%BladeMotion(k)%RefOrientation(:,:,j), wm_crv, ErrStat2, ErrMsg2)
!          call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
!          p%DX_p%bldRefPos((jTot-1)*6+1:(jTot-1)*6+3) = u%BladeMotion(k)%Position(:,j)
!          p%DX_p%bldRefPos((jTot-1)*6+4:(jTot-1)*6+6) = wm_crv
!          jTot = jTot+1
!       end do
!    end do

!    call BD_CrvExtractCrv(u%HubMotion%RefOrientation(:,:,1), wm_crv, ErrStat2, ErrMsg2)
!    call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
!    p%DX_p%hubRefPos(1:3) = u%HubMotion%Position(:,1)
!    p%DX_p%hubRefPos(4:6) = wm_crv

!    call BD_CrvExtractCrv(u%NacelleMotion%RefOrientation(:,:,1), wm_crv, ErrStat2, ErrMsg2)
!    call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
!    p%DX_p%nacRefPos(1:3) = u%NacelleMotion%Position(:,1)
!    p%DX_p%nacRefPos(4:6) = wm_crv

!    do k=1,p%NumBlds
!       call BD_CrvExtractCrv(u%BladeRootMotion(k)%RefOrientation(:,:,1), wm_crv, ErrStat2, ErrMsg2)
!       call SetErrStat(ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
!       p%DX_p%bldRootRefPos((k-1)*6+1:(k-1)*6+3) = u%BladeRootMotion(k)%Position(:,1)
!       p%DX_p%bldRootRefPos((k-1)*6+4:(k-1)*6+6) = wm_crv
!    end do
      

!    ! Now the displacements
!    CALL AllocPAry( u%DX_u%twrDef, p%NumTwrNds*12, 'twrDef', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( u%DX_u%bldDef, p%nTotBldNds*12, 'bldDef', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( u%DX_u%hubDef, 12, 'hubDef', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( u%DX_u%nacDef, 12, 'nacDef', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( u%DX_u%bldRootDef, p%NumBlds*12, 'bldRootDef', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   
!    ! make sure the C versions are synced with these arrays
!    u%DX_u%c_obj%twrDef_Len = p%NumTwrNds*12; u%DX_u%c_obj%twrDef = C_LOC( u%DX_u%twrDef(1) )
!    u%DX_u%c_obj%bldDef_Len = p%nTotBldNds*12; u%DX_u%c_obj%bldDef = C_LOC( u%DX_u%bldDef(1) )
!    u%DX_u%c_obj%hubDef_Len = 12; u%DX_u%c_obj%hubDef = C_LOC( u%DX_u%hubDef(1) )
!    u%DX_u%c_obj%nacDef_Len = 12; u%DX_u%c_obj%nacDef = C_LOC( u%DX_u%nacDef(1) )
!    u%DX_u%c_obj%bldRootDef_Len = p%NumBlds*12; u%DX_u%c_obj%bldRootDef = C_LOC( u%DX_u%bldRootDef(1) )
!    call ExtPtfmLd_ConvertInpDataForExtProg(u, p, ErrStat2, ErrMsg2 )
!    call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

!    CALL AllocPAry( p%DX_p%bldChord, p%nTotBldNds, 'bldChord', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( p%DX_p%bldRloc, p%nTotBldNds, 'bldRloc', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( p%DX_p%twrdia, p%NumTwrNds, 'twrDia', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( p%DX_p%twrHloc, p%NumTwrNds, 'twrHloc', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!    CALL AllocPAry( u%DX_u%bldPitch, p%NumBlds, 'bldPitch', ErrStat2, ErrMsg2 ); CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

!    ! make sure the C versions are synced with these arrays
!    p%DX_p%c_obj%bldChord_Len = p%nTotBldNds; p%DX_p%c_obj%bldChord = C_LOC( p%DX_p%bldChord(1) )
!    p%DX_p%c_obj%bldRloc_Len = p%nTotBldNds; p%DX_p%c_obj%bldRloc = C_LOC( p%DX_p%bldRloc(1) )
!    p%DX_p%c_obj%twrDia_Len = p%NumTwrNds; p%DX_p%c_obj%twrDia = C_LOC( p%DX_p%twrDia(1) )
!    p%DX_p%c_obj%twrHloc_Len = p%NumTwrNds; p%DX_p%c_obj%twrHloc = C_LOC( p%DX_p%twrHloc(1) )
!    u%DX_u%c_obj%bldPitch_Len = p%NumBlds; u%DX_u%c_obj%bldPitch = C_LOC( u%DX_u%bldPitch(1) )

!    jTot = 1
!    do k=1,p%NumBlds
!       do j=1,p%NumBldNds(k)
!          p%DX_p%bldChord(jTot) = InitInp%bldChord(j,k)
!          p%DX_p%bldRloc(jTot) = InitInp%bldRloc(j,k)
!          jTot = jTot+1
!       end do
!    end do

!    do j=1,p%NumTwrNds
!       p%DX_p%twrDia(j) = InitInp%twrDia(j)
!       p%DX_p%twrHloc(j) = InitInp%twrHloc(j)
!    end do
   
! end subroutine Init_u
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine converts the displacement data in the meshes in the input into a simple array format that can be accessed by external programs
subroutine ExtPtfmLd_ConvertInpDataForExtProg(u, p, errStat, errMsg )
!..................................................................................................................................
  USE BeamDyn_IO, ONLY: BD_CrvExtractCrv
  
   type(ExtPtfmLd_InputType),           intent(inout)  :: u                 !< Input data
   type(ExtPtfmLd_ParameterType),       intent(in   )  :: p                 !< Parameters
   integer(IntKi),               intent(  out)  :: errStat           !< Error status of the operation
   character(*),                 intent(  out)  :: errMsg            !< Error message if ErrStat /= ErrID_None


      ! Local variables
   real(R8Ki)                                   :: wm_crv(3)         ! Wiener-Milenkovic parameters
   integer(intKi)                               :: j                 ! counter for nodes
   integer(intKi)                               :: jTot              ! counter for nodes
   integer(intKi)                               :: k                 ! counter for blades
   real(reki)                                   :: cref(3)
   real(reki)                                   :: xloc(3)
   real(reki)                                   :: yloc(3)
   real(reki)                                   :: zloc(3)
   
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'ExtPtfmLd_ConvertInpDataForExtProg'

      ! Initialize variables for this routine

   ErrStat = ErrID_None
   ErrMsg  = ""

   u%DX_u%ptfmDef(1:3) = u%PtfmMotion%TranslationDisp(:, 1)
   u%DX_u%ptfmDef(4:6) = u%PtfmMotion%Orientation(:, 1, 1)
   u%DX_u%ptfmDef(7:9) = u%PtfmMotion%Orientation(:, 2, 1)
   u%DX_u%ptfmDef(10:12) = u%PtfmMotion%Orientation(:, 3, 1)
   u%DX_u%ptfmDef(13:15) = u%PtfmMotion%TranslationVel(:, 1)
   u%DX_u%ptfmDef(16:18) = u%PtfmMotion%RotationVel(:, 1)
   u%DX_u%ptfmDef(19:21) = u%PtfmMotion%TranslationAcc(:, 1)
   u%DX_u%ptfmDef(22:24) = u%PtfmMotion%RotationAcc(:, 1)

   ! print *, "---- Platform State ----"
   ! write(*,'(A,3F10.5)') "Displacement (m): ", u%PtfmMotion%TranslationDisp(:, 1)

   ! print *, "Orientation matrix:"
   ! write(*,'(3F10.5)') u%PtfmMotion%Orientation(:, 1, 1)
   ! write(*,'(3F10.5)') u%PtfmMotion%Orientation(:, 2, 1)
   ! write(*,'(3F10.5)') u%PtfmMotion%Orientation(:, 3, 1)

   ! write(*,'(A,3F10.5)') "Trans Vel (m/s): ", u%PtfmMotion%TranslationVel(:, 1)
   ! write(*,'(A,3F10.5)') "Rot Vel (rad/s): ", u%PtfmMotion%RotationVel(:, 1)
   ! print *, "------------------------"
   ! print *,"C_LOC(u%DX_u%ptfmDef(1)) = ", C_LOC(u%DX_u%ptfmDef(1))
   ! write(*, '(A, 3F10.5)') "u.DX_u.ptfmDef: ", u%DX_u%ptfmDef(1:3)

   ! if (p%TwrAero) then
   !    do j=1,p%NumTwrNds
   !       call BD_CrvExtractCrv(u%TowerMotion%Orientation(:,:,j), wm_crv, ErrStat2, ErrMsg2)
   !       call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   !       u%DX_u%twrDef((j-1)*12+1:(j-1)*12+3) = u%TowerMotion%TranslationDisp(:,j)
   !       u%DX_u%twrDef((j-1)*12+4:(j-1)*12+6) = u%TowerMotion%TranslationVel(:,j)
   !       u%DX_u%twrDef((j-1)*12+7:(j-1)*12+9) = wm_crv
   !       u%DX_u%twrDef((j-1)*12+10:(j-1)*12+12) = u%TowerMotion%RotationVel(:,j)
   !    end do
   ! end if

   ! jTot = 1
   ! do k=1,p%NumBlds
   !    do j=1,p%NumBldNds(k)
   !       call BD_CrvExtractCrv(u%BladeMotion(k)%Orientation(:,:,j), wm_crv, ErrStat2, ErrMsg2)
   !       call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)

   !       u%DX_u%bldDef((jTot-1)*12+1:(jTot-1)*12+3) = u%BladeMotion(k)%TranslationDisp(:,j)
   !       u%DX_u%bldDef((jTot-1)*12+4:(jTot-1)*12+6) = u%BladeMotion(k)%TranslationVel(:,j)
   !       u%DX_u%bldDef((jTot-1)*12+7:(jTot-1)*12+9) = wm_crv
   !       u%DX_u%bldDef((jTot-1)*12+10:(jTot-1)*12+12) = u%BladeMotion(k)%RotationVel(:,j)
   !       jTot = jTot+1
   !    end do
   ! end do
      
   ! call BD_CrvExtractCrv(u%HubMotion%Orientation(:,:,1), wm_crv, ErrStat2, ErrMsg2)
   ! call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   ! u%DX_u%hubDef(1:3) = u%HubMotion%TranslationDisp(:,1)
   ! u%DX_u%hubDef(4:6) = u%HubMotion%TranslationVel(:,1)
   ! u%DX_u%hubDef(7:9) = wm_crv
   ! u%DX_u%hubDef(10:12) = u%HubMotion%RotationVel(:,1)

   ! call BD_CrvExtractCrv(u%NacelleMotion%Orientation(:,:,1), wm_crv, ErrStat2, ErrMsg2)
   ! call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   ! u%DX_u%nacDef(1:3) = u%NacelleMotion%TranslationDisp(:,1)
   ! u%DX_u%nacDef(4:6) = u%NacelleMotion%TranslationVel(:,1)
   ! u%DX_u%nacDef(7:9) = wm_crv
   ! u%DX_u%nacDef(10:12) = u%NacelleMotion%RotationVel(:,1)

   ! do k=1,p%NumBlds
   !    call BD_CrvExtractCrv(u%BladeRootMotion(k)%Orientation(:,:,1), wm_crv, ErrStat2, ErrMsg2)
   !    call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName)
   !    u%DX_u%bldRootDef( (k-1)*12+1:(k-1)*12+3 ) = u%BladeRootMotion(k)%TranslationDisp(:,1)
   !    u%DX_u%bldRootDef( (k-1)*12+4:(k-1)*12+6 ) = u%BladeRootMotion(k)%TranslationVel(:,1)
   !    u%DX_u%bldRootDef( (k-1)*12+7:(k-1)*12+9 ) = wm_crv
   !    u%DX_u%bldRootDef( (k-1)*12+10:(k-1)*12+12 ) = u%BladeRootMotion(k)%RotationVel(:,1)
   ! end do
   
end subroutine ExtPtfmLd_ConvertInpDataForExtProg
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine converts the data in the simple array format in the output data type into OpenFAST mesh format
subroutine ExtPtfmLd_ConvertOpDataForOpenFAST(y, u, m, p, errStat, errMsg )
!..................................................................................................................................
  
   type(ExtPtfmLd_OutputType),          intent(inout)  :: y                 !< Ouput data
   type(ExtPtfmLd_InputType),           intent(in   )  :: u                 !< Input data
   type(ExtPtfmLd_MiscVarType),         intent(inout)  :: m                 !< Misc var
   type(ExtPtfmLd_ParameterType),       intent(in   )  :: p                 !< Parameters
   integer(IntKi),               intent(  out)  :: errStat           !< Error status of the operation
   character(*),                 intent(  out)  :: errMsg            !< Error message if ErrStat /= ErrID_None


      ! Local variables
   integer(intKi)                               :: j                 ! counter for nodes
   integer(intKi)                               :: jTot              ! counter for nodes
   integer(intKi)                               :: k                 ! counter for blades
   real(ReKi)                                   :: tmp_az, delta_az  ! temporary variable for azimuth
   
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'ExtPtfmLd_ConvertInpDataForExtProg'

      ! Initialize variables for this routine

   ErrStat = ErrID_None
   ErrMsg  = ""

   y%PtfmMesh%Force(:, 1) = y%DX_y%ptfmLd(1:3)
   y%PtfmMesh%Moment(:, 1) = y%DX_y%ptfmLd(4:6)

   ! tmp_az = m%az
   ! call Zero2TwoPi(tmp_az)
   ! delta_az = u%az - tmp_az
   ! if ( delta_az .lt. -1.0 )  then
   !    m%az = m%az + delta_az + PI
   ! else
   !    m%az = m%az + delta_az
   ! end if
   ! if (m%az  > (p%az_blend_mean  - 0.5 * p%az_blend_delta)) then
   !    m%phi_cfd = 0.5 * ( tanh( (m%az - p%az_blend_mean)/p%az_blend_delta ) + 1.0 )
   ! else
   !    m%phi_cfd = 0.0
   ! end if

   ! if (p%TwrAero) then
   !    do j=1,p%NumTwrNds
   !       y%TowerLoad%Force(:,j) = m%phi_cfd * y%DX_y%twrLd((j-1)*6+1:(j-1)*6+3) + (1.0 - m%phi_cfd) * y%TowerLoadAD%Force(:,j)
   !       y%TowerLoad%Moment(:,j) = m%phi_cfd * y%DX_y%twrLd((j-1)*6+4:(j-1)*6+6) + (1.0 - m%phi_cfd) * y%TowerLoadAD%Moment(:,j)
   !    end do
   ! end if

   ! jTot = 1
   ! do k=1,p%NumBlds
   !    do j=1,p%NumBldNds(k)
   !       y%BladeLoad(k)%Force(:,j) = m%phi_cfd * y%DX_y%bldLd((jTot-1)*6+1:(jTot-1)*6+3) + (1.0 - m%phi_cfd) * y%BladeLoadAD(k)%Force(:,j)
   !       y%BladeLoad(k)%Moment(:,j) = m%phi_cfd * y%DX_y%bldLd((jTot-1)*6+4:(jTot-1)*6+6) + (1.0 - m%phi_cfd) * y%BladeLoadAD(k)%Moment(:,j)
   !       jTot = jTot+1
   !    end do
   ! end do
   
   
end subroutine ExtPtfmLd_ConvertOpDataForOpenFAST
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine is called at the end of the simulation.
subroutine ExtPtfmLd_End( u, p, x, xd, z, OtherState, y, m, ErrStat, ErrMsg )
!..................................................................................................................................

      TYPE(ExtPtfmLd_InputType),           INTENT(INOUT)  :: u           !< System inputs
      TYPE(ExtPtfmLd_ParameterType),       INTENT(INOUT)  :: p           !< Parameters
      TYPE(ExtPtfmLd_ContinuousStateType), INTENT(INOUT)  :: x           !< Continuous states
      TYPE(ExtPtfmLd_DiscreteStateType),   INTENT(INOUT)  :: xd          !< Discrete states
      TYPE(ExtPtfmLd_ConstraintStateType), INTENT(INOUT)  :: z           !< Constraint states
      TYPE(ExtPtfmLd_OtherStateType),      INTENT(INOUT)  :: OtherState  !< Other states
      TYPE(ExtPtfmLd_OutputType),          INTENT(INOUT)  :: y           !< System outputs
      TYPE(ExtPtfmLd_MiscVarType),         INTENT(INOUT)  :: m           !< Misc/optimization variables
      INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     !< Error status of the operation
      CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None



         ! Initialize ErrStat

      ErrStat = ErrID_None
      ErrMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:


         ! Destroy the input data:


END SUBROUTINE ExtPtfmLd_End
!----------------------------------------------------------------------------------------------------------------------------------
!> Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete and other states.
! Continuous, constraint, discrete, and other states are updated for t + Interval
subroutine ExtPtfmLd_UpdateStates( t, n, u, utimes, p, x, xd, z, OtherState, m, errStat, errMsg )
!..................................................................................................................................

   real(DbKi),                     intent(in   ) :: t          !< Current simulation time in seconds
   integer(IntKi),                 intent(in   ) :: n          !< Current simulation time step n = 0,1,...
   type(ExtPtfmLd_InputType),             intent(inout) :: u(:)       !< Inputs at utimes (out only for mesh record-keeping in ExtrapInterp routine)
   real(DbKi),                     intent(in   ) :: utimes(:)  !< Times associated with u(:), in seconds
   type(ExtPtfmLd_ParameterType),         intent(in   ) :: p          !< Parameters
   type(ExtPtfmLd_ContinuousStateType),   intent(inout) :: x          !< Input: Continuous states at t;
                                                               !!   Output: Continuous states at t + Interval
   type(ExtPtfmLd_DiscreteStateType),     intent(inout) :: xd         !< Input: Discrete states at t;
                                                               !!   Output: Discrete states at t  + Interval
   type(ExtPtfmLd_ConstraintStateType),   intent(inout) :: z          !< Input: Constraint states at t;
                                                               !!   Output: Constraint states at t+dt
   type(ExtPtfmLd_OtherStateType),        intent(inout) :: OtherState !< Input: Other states at t;
                                                               !!   Output: Other states at t+dt
   type(ExtPtfmLd_MiscVarType),           intent(inout) :: m          !< Misc/optimization variables
   integer(IntKi),                 intent(  out) :: errStat    !< Error status of the operation
   character(*),                   intent(  out) :: errMsg     !< Error message if ErrStat /= ErrID_None

   ! local variables
   type(ExtPtfmLd_InputType)                           :: uInterp     ! Interpolated/Extrapolated input
   integer(intKi)                               :: ErrStat2          ! temporary Error status
   character(ErrMsgLen)                         :: ErrMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'ExtPtfmLd_UpdateStates'
      
   ErrStat = ErrID_None
   ErrMsg  = ""
           
   
end subroutine ExtPtfmLd_UpdateStates
!----------------------------------------------------------------------------------------------------------------------------------
!> Routine for computing outputs, used in both loose and tight coupling.
!! This subroutine is used to compute the output channels (motions and loads) and place them in the WriteOutput() array.
!! The descriptions of the output channels are not given here. Please see the included OutListParameters.xlsx sheet for
!! for a complete description of each output parameter.
subroutine ExtPtfmLd_CalcOutput( t, u, p, x, xd, z, OtherState, y, m, ErrStat, ErrMsg )
! NOTE: no matter how many channels are selected for output, all of the outputs are calculated
! All of the calculated output channels are placed into the m%AllOuts(:), while the channels selected for outputs are
! placed in the y%WriteOutput(:) array.
!..................................................................................................................................

   REAL(DbKi),                   INTENT(IN   )  :: t           !< Current simulation time in seconds
   TYPE(ExtPtfmLd_InputType),           INTENT(IN   )  :: u           !< Inputs at Time t
   TYPE(ExtPtfmLd_ParameterType),       INTENT(IN   )  :: p           !< Parameters
   TYPE(ExtPtfmLd_ContinuousStateType), INTENT(IN   )  :: x           !< Continuous states at t
   TYPE(ExtPtfmLd_DiscreteStateType),   INTENT(IN   )  :: xd          !< Discrete states at t
   TYPE(ExtPtfmLd_ConstraintStateType), INTENT(IN   )  :: z           !< Constraint states at t
   TYPE(ExtPtfmLd_OtherStateType),      INTENT(IN   )  :: OtherState  !< Other states at t
   TYPE(ExtPtfmLd_OutputType),          INTENT(INOUT)  :: y           !< Outputs computed at t (Input only so that mesh con-
                                                               !!   nectivity information does not have to be recalculated)
   type(ExtPtfmLd_MiscVarType),         intent(inout)  :: m           !< Misc/optimization variables
   INTEGER(IntKi),               INTENT(  OUT)  :: ErrStat     !< Error status of the operation
   CHARACTER(*),                 INTENT(  OUT)  :: ErrMsg      !< Error message if ErrStat /= ErrID_None


   integer, parameter                           :: indx = 1  ! m%BEMT_u(1) is at t; m%BEMT_u(2) is t+dt
   integer(intKi)                               :: i
   integer(intKi)                               :: j

   integer(intKi)                               :: ErrStat2
   character(ErrMsgLen)                         :: ErrMsg2
   character(*), parameter                      :: RoutineName = 'ExtPtfmLd_CalcOutput'

   ErrStat = ErrID_None
   ErrMsg  = ""

 end subroutine ExtPtfmLd_CalcOutput

END MODULE ExtPtfmLoads
