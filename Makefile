TST_DIR = test
SRC_DIR = src
MAP_DIR = maps

CFLAGS= -Wall -Wextra -Werrror -std=c99 -g3

all : test server report

test : ${TST_DIR}/all-tests.rkt ${TST_DIR}/test_graph_interface.rkt ${TST_DIR}/test_parse.rkt
	racket $<

server : ${SRC_DIR}/server.rkt ${MAP_DIR}/aprojMapping.osm
	racket $^

clean :
	rm -rf ${SRC_DIR}/compiled ${TST_DIR}/compiled


