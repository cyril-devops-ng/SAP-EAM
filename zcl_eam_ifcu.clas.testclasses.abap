*"* use this source file for your ABAP unit test classes
class ltcl_eam_ifcu definition final for testing
  duration LONG
  risk level harmless.

  private section.
    methods:
      test_map_ifcu_process for testing raising cx_static_check,
      test_authorization_check for testing raising cx_static_check.

endclass.


class ltcl_eam_ifcu implementation.

  method test_authorization_check.

  endmethod.

  method test_map_ifcu_process.

  endmethod.

endclass.
