default: user_guide

all:developer_reference user_guide

clean:
	${MAKE} -C devel/ide $@
	${MAKE} -C user $@

developer_reference:
	${MAKE} -C devel/ide $@

user_guide:
	${MAKE} -C user $@

.PHONY:all developer_reference user_guide clean
