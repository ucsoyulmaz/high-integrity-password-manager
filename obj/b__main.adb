pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__main.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E015 : Short_Integer; pragma Import (Ada, E015, "system__soft_links_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "system__exception_table_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "system__exceptions_E");
   E017 : Short_Integer; pragma Import (Ada, E017, "system__soft_links__initialize_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "ada__containers_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "ada__io_exceptions_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "ada__strings_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "system__os_lib_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "ada__tags_E");
   E058 : Short_Integer; pragma Import (Ada, E058, "ada__streams_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "system__file_control_block_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "system__finalization_root_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "ada__finalization_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "system__file_io_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "system__storage_pools_E");
   E124 : Short_Integer; pragma Import (Ada, E124, "system__finalization_masters_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "system__storage_pools__subpools_E");
   E056 : Short_Integer; pragma Import (Ada, E056, "ada__text_io_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "ada__strings__maps_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "ada__strings__unbounded_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "mycommandline_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "mystring_E");
   E142 : Short_Integer; pragma Import (Ada, E142, "mystringtokeniser_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "numericcontroller_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "passworddatabase_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "pin_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "passwordmanager_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E146 := E146 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "passworddatabase__finalize_spec");
      begin
         F1;
      end;
      E108 := E108 - 1;
      declare
         procedure F2;
         pragma Import (Ada, F2, "ada__strings__unbounded__finalize_spec");
      begin
         F2;
      end;
      E056 := E056 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "ada__text_io__finalize_spec");
      begin
         F3;
      end;
      E122 := E122 - 1;
      declare
         procedure F4;
         pragma Import (Ada, F4, "system__storage_pools__subpools__finalize_spec");
      begin
         F4;
      end;
      E124 := E124 - 1;
      declare
         procedure F5;
         pragma Import (Ada, F5, "system__finalization_masters__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "system__file_io__finalize_body");
      begin
         E077 := E077 - 1;
         F6;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E025 := E025 + 1;
      System.Exceptions'Elab_Spec;
      E027 := E027 + 1;
      System.Soft_Links.Initialize'Elab_Body;
      E017 := E017 + 1;
      E015 := E015 + 1;
      Ada.Containers'Elab_Spec;
      E147 := E147 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E059 := E059 + 1;
      Ada.Strings'Elab_Spec;
      E106 := E106 + 1;
      System.Os_Lib'Elab_Body;
      E082 := E082 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Tags'Elab_Body;
      E061 := E061 + 1;
      Ada.Streams'Elab_Spec;
      E058 := E058 + 1;
      System.File_Control_Block'Elab_Spec;
      E085 := E085 + 1;
      System.Finalization_Root'Elab_Spec;
      E080 := E080 + 1;
      Ada.Finalization'Elab_Spec;
      E078 := E078 + 1;
      System.File_Io'Elab_Body;
      E077 := E077 + 1;
      System.Storage_Pools'Elab_Spec;
      E130 := E130 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E124 := E124 + 1;
      System.Storage_Pools.Subpools'Elab_Spec;
      E122 := E122 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E056 := E056 + 1;
      Ada.Strings.Maps'Elab_Spec;
      E112 := E112 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E108 := E108 + 1;
      E138 := E138 + 1;
      E140 := E140 + 1;
      E142 := E142 + 1;
      E144 := E144 + 1;
      Passworddatabase'Elab_Spec;
      E146 := E146 + 1;
      E154 := E154 + 1;
      E152 := E152 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_main");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /Users/cemsoyulmaz/Desktop/zac-task3/obj/mycommandline.o
   --   /Users/cemsoyulmaz/Desktop/zac-task3/obj/mystring.o
   --   /Users/cemsoyulmaz/Desktop/zac-task3/obj/mystringtokeniser.o
   --   /Users/cemsoyulmaz/Desktop/zac-task3/obj/numericcontroller.o
   --   /Users/cemsoyulmaz/Desktop/zac-task3/obj/passworddatabase.o
   --   /Users/cemsoyulmaz/Desktop/zac-task3/obj/pin.o
   --   /Users/cemsoyulmaz/Desktop/zac-task3/obj/passwordmanager.o
   --   /Users/cemsoyulmaz/Desktop/zac-task3/obj/main.o
   --   -L/Users/cemsoyulmaz/Desktop/zac-task3/obj/
   --   -L/Users/cemsoyulmaz/Desktop/zac-task3/obj/
   --   -L/users/cemsoyulmaz/opt/gnat/2019/lib/gcc/x86_64-apple-darwin17.7.0/8.3.1/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end ada_main;
