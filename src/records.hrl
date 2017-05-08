-ifndef(IERL_RECORDS_HRL).
-define(IERL_RECORDS_HRL, 1).

-record(ierl_connection_file, {
          transport :: atom(),
          ip :: binary(),

          control_port :: integer(),
          heartbeat_port :: integer(),
          shell_port :: integer(),
          stdin_port :: integer(),
          iopub_port :: integer(),

          signature_key :: {crypto:hash_algorithms(), binary()}
         }).


-record(ierl_msg, {
          uuids = []          :: [binary()],
          header = #{}        :: map(),
          parent_header = #{} :: map(),
          metadata = #{}      :: map(),
          content = #{}       :: map(),
          extra_binaries = [] :: [binary()]
         }).

-endif.
