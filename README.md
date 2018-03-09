# ranch_tcp
ranch_tcp is a socket acceptor pool for TCP protocols
# user guide
1. 启动ranch_tcp

   cd ranch_tcp
   erl -pa ./ebin -pa ./deps/godlrush/ebin -pa ./deps/lager/ebin -s ranch_tcp_app start
2. 监听端口

   just run like `test:test_run_1` or `test:test_run_2`

3. client connect

   echo client like `test:test_echo(5)`
