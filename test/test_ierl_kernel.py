import unittest
import jupyter_kernel_test as jkt

class IErlangKernelTests(jkt.KernelTests):
    kernel_name = "ierlang"

    language_name = "erlang"

    file_extension = ".erl"

    code_hello_world = 'io:format("hello, world~n").'

    completion_samples = [
        {
            'text': 'lists:',
            'matches': {'lists:split('},
        },
    ]

    complete_code_samples = [
        '1.',
        'io:format("asdf").',
        "Func = fun (X) -> X * X end.",
    ]

    incomplete_code_samples = ["io:format(", "Func = fun () ->"]

    code_generate_error = "error(blubb)."

    code_execute_result = [
        {'code': "1+2+3.", 'result': "6"}
    ]

    code_display_data = [
    ]


if __name__ == '__main__':
    unittest.main()
