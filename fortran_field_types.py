
# Copyright (c) 2018 Science and Technology Facilities Council

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

'''A simple fparser2 Fortran2008 example demonstrating support for
submodules'''
import copy
from ast import NodeTransformer
from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
from fparser.two import pattern_tools
from fparser.two.utils import walk, get_child
# pylint: disable=no-name-in-module
from fparser.two.Fortran2003 import Main_Program, Module, \
    Subroutine_Subprogram, Function_Subprogram, Module_Stmt, Use_Stmt, Call_Stmt, \
    Actual_Arg_Spec, Data_Ref, Part_Ref, Char_Literal_Constant, \
    Section_Subscript_List, Name, Real_Literal_Constant, Int_Literal_Constant,\
    Function_Reference, Level_2_Unary_Expr, Add_Operand, Parenthesis,\
    Type_Declaration_Stmt, Type_Declaration_StmtBase, Type_Guard_Stmt, Type_Name,\
    Type_Param_Name, Declaration_Type_Spec, Entity_Decl,End_Module_Stmt, \
    Derived_Type_Stmt, End_Type_Stmt, Kind_Selector


## The dummy fortran file
MYFILE = '''
module MODNAME
  implicit none
  type, public :: BASE_FIELD_TYPE
     real(kind=RKIND) :: data
   contains
     procedure :: mifunc
  end type BASE_FIELD_TYPE
contains
  function mifunc(self, arg2) result(ans)
    implicit none
    class(BASE_FIELD_TYPE), intent(in) :: self
    real(kind=r_def), intent(in) :: arg2
    real(kind=RKIND) :: ans
    
    ans=self%data+arg2
    return ans
  end function mifunc
end module MODNAME
'''

# The configurable list of substitutions
mods='field_mod','rsolver_field_mod'
types='field_type','rsolver_field_type'
kinds='r_def','r_solver_def'


# Loop over configurable list
for i in range(0,2):

    # Parse the fortran into an AST
    READER = FortranStringReader(MYFILE)
    F2008_PARSER = ParserFactory().create(std="f2008")
    past = F2008_PARSER(READER)

    # Walk through the content
    for statement in walk(past.content):
        # Assume everything is wrapped in a module
        # There are three cases to catch, the (end) module statement
        # The (end) type statement
        # The kind statements
        # In in case, the fparser2 object is a tuple. Convert it to a list
        # after check, replace the text (in each case item 1 from list)
        # from the substitution list
        # Convert back to a tuple in the AST
        if isinstance(statement, Module):
            # walk through the statements of the module
            for child in walk(statement):
                # If it is the (end) module statement
                if isinstance(child, Module_Stmt) or isinstance(child, End_Module_Stmt):
                    children=list(child.items)
                    children[1]=mods[i]
                    child.items=tuple(children)
                # If is the (end) of the type declaration    
                elif isinstance(child, Derived_Type_Stmt) or isinstance(child, End_Type_Stmt) or isinstance(child, Declaration_Type_Spec):
                    children=list(child.items)
                    name=str(children[1])
                    if name=='BASE_FIELD_TYPE':
                        children[1]=types[i]
                        child.items=tuple(children)
                # If it is the kind statement        
                elif isinstance(child, Kind_Selector):
                    children=list(child.items)
                    name=str(children[1])
                    if name == 'RKIND':
                        children[1]=kinds[i]
                        child.items=tuple(children)

    print(past)
    print("--")
