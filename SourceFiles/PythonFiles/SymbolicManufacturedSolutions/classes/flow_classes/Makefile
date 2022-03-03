# Makefile for python codes
#
# 

VENV = venv
PYTHON = $(VENV)/bin/python3
PIP = $(VENV)/bin/pip

run: $(VENV)/bin/activate
	 @$(PYTHON) example.py


$(VENV)/bin/activate: requirements.txt
	 python3 -m venv $(VENV)
	  $(PIP) install -r requirements.txt

test:
	pytest test_flow_class.py

clean:
	 rm -rf __pycache__
	 rm -rf $(VENV)
	 rm *.log
