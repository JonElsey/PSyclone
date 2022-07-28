# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         A. B. G. Chalk, STFC Daresbury Lab
#         I. Kavcic,    Met Office
#         C.M. Maynard, Met Office / University of Reading
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the various OpenMP Directive
nodes.'''


from __future__ import absolute_import
import abc
import itertools
import math
import six

from psyclone.configuration import Config
from psyclone.core import AccessType, VariablesAccessInfo
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import (AssignGen, UseGen, DeclGen, DirectiveGen,
                              CommentGen)
from psyclone.psyir.nodes import Reference, Assignment, IfBlock, Loop, \
                                 ArrayReference, ArrayOfStructuresReference, \
                                 StructureReference, Literal, Call, \
                                 CodeBlock, Node
from psyclone.psyir.nodes.operation import BinaryOperation
from psyclone.psyir.nodes.directive import StandaloneDirective, \
    RegionDirective
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.omp_clauses import OMPGrainsizeClause, \
    OMPNowaitClause, OMPNogroupClause, OMPNumTasksClause, OMPPrivateClause,\
    OMPDefaultClause, OMPReductionClause, OMPScheduleClause,\
    OMPFirstprivateClause, OMPDependClause, OMPSharedClause
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import INTEGER_TYPE

# OMP_OPERATOR_MAPPING is used to determine the operator to use in the
# reduction clause of an OpenMP directive.
OMP_OPERATOR_MAPPING = {AccessType.SUM: "+"}


class OMPDirective(metaclass=abc.ABCMeta):
    '''
    Base mixin class for all OpenMP-related directives.

    This class is useful to provide a unique common ancestor to all the
    OpenMP directives, for instance when traversing the tree with
    `node.walk(OMPDirective)`

    Note that classes inheriting from it must place the OMPDirective in
    front of the other Directive node sub-class, so that the Python
    MRO gives preference to this class's attributes.
    '''
    _PREFIX = "OMP"


@six.add_metaclass(abc.ABCMeta)
class OMPRegionDirective(OMPDirective, RegionDirective):
    '''
    Base class for all OpenMP region-related directives.

    '''
    def _get_reductions_list(self, reduction_type):
        '''
        Returns the names of all scalars within this region that require a
        reduction of type 'reduction_type'. Returned names will be unique.

        :param reduction_type: the reduction type (e.g. AccessType.SUM) to \
                               search for.
        :type reduction_type: :py:class:`psyclone.core.access_type.AccessType`

        :returns: names of scalar arguments with reduction access.
        :rtype: list of str

        '''
        result = []
        const = Config.get().api_conf().get_constants()
        for call in self.kernels():
            if not call.arguments:
                continue
            for arg in call.arguments.args:
                if arg.argument_type in const.VALID_SCALAR_NAMES:
                    if arg.descriptor.access == reduction_type:
                        if arg.name not in result:
                            result.append(arg.name)
        return result


@six.add_metaclass(abc.ABCMeta)
class OMPStandaloneDirective(OMPDirective, StandaloneDirective):
    '''
    Base class for all OpenMP-related standalone directives

    '''


class OMPDeclareTargetDirective(OMPStandaloneDirective):
    '''
    Class representing an OpenMP Declare Target directive in the PSyIR.

    '''
    def gen_code(self, parent):
        '''Generate the fortran OMP Declare Target Directive and any
        associated code.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        # Check the constraints are correct
        self.validate_global_constraints()

        # Generate the code for this Directive
        parent.add(DirectiveGen(parent, "omp", "begin", "declare", "target"))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp routine". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp declare target"

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this directive is not the first statement \
            in a routine.

        '''
        if self.parent and (not isinstance(self.parent, Routine) or
                            self.parent.children[0] is not self):
            raise GenerationError(
                f"A OMPDeclareTargetDirective must be the first child (index "
                f"0) of a Routine but found one as child {self.position} of a "
                f"{type(self.parent).__name__}.")

        super().validate_global_constraints()


class OMPTaskwaitDirective(OMPStandaloneDirective):
    '''
    Class representing an OpenMP TASKWAIT directive in the PSyIR.

    '''
    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPTaskwait is not enclosed \
                            within some OpenMP parallel region.

        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As a Parallel Child
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                "OMPTaskwaitDirective must be inside an OMP parallel region "
                "but could not find an ancestor OMPParallelDirective node")

        super(OMPTaskwaitDirective, self).validate_global_constraints()

    def gen_code(self, parent):
        '''Generate the fortran OMP Taskwait Directive and any associated
        code

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        # Check the constraints are correct
        self.validate_global_constraints()

        # Generate the code for this Directive
        parent.add(DirectiveGen(parent, "omp", "begin", "taskwait", ""))
        # No children or end code for this node

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp taskwait". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp taskwait"


@six.add_metaclass(abc.ABCMeta)
class OMPSerialDirective(OMPRegionDirective):
    '''
    Abstract class representing OpenMP serial regions, e.g.
    OpenMP SINGLE or OpenMP Master.

    '''

    def _validate_satisfiable_task_dependencies(self):
        '''
        Perform validation on all child OMPTaskDirectives to ensure that 
        all of their dependencies are satisfiable under OpenMP 5.0's task
        dependency rules.

        :raises GenerationError: if unsatisfiable task dependencies exist.
        '''
        # Avoid circular dependency
        from psyclone.psyGen import CodedKern
        # Find all of the task children of this node.
        tasks = self.walk(OMPTaskDirective)
        if len(tasks) == 0:
            return
      
        # Setup a dict to store the abs positions of nodes so we don't
        # recompute them every time we need to check them.
        self._node_abs_positions = {}
        self._this_nodes_position = self.abs_position

        # Store a list of all nodes in the region. We store all (as opposed to
        # only Loop/Assignment nodes as it is difficult to start the walk
        # from a specific point without all nodes.
        self._nodes_in_region = self.walk(Node)

        var_info = VariablesAccessInfo(self)


        # Find all the combinations of tasks
        task_pairs = itertools.combinations(tasks, 2)

        for task_pair in task_pairs:
            task1 = task_pair[0]
            task2 = task_pair[1]
            task1_ins = task1.children[4]
            task2_ins = task2.children[4]
            task1_outs = task1.children[5]
            task2_outs = task2.children[5]

            # For each input symbol to task 1, check whether its used in task 2
            for invar in task1_ins.children:
                for var2 in zip(task2_ins.children, task2_outs.children):
                    if invar.symbol == var2.symbol:
                        # Only an issue for Array[OfStructure]Reference so skip
                        # other types
                        if not isinstance(invar, ArrayMixin):
                            continue
                        # Have an array, need to compare indices to check if
                        # they are compatible.
                        for count, index1 in enumerate(invar.indices):
                            index2 = var2.indices[count]
                            if (isinstance(index1, Literal) and 
                                index1 != index2):
                                # This condition is actually overly tight.
                                # A reference to a constant can validly
                                # be equivalent to a Literal if we know
                                # the value of that constant.
                                assert False # Throw exception
                            if isinstance(index1, Reference):
                                # For now we reject Reference vs Literal
                                if isinstance(index2, Literal):
                                    assert False # Throw exception

                                # Once we have a reference or Binary Operation
                                # we need to compute the possible values it
                                # might take.
                                
                                # Find the previous access to this
                                # Start walking backwards from this node's position
                                last_write_1 = None
                                task1_position = self._node_abs_positions.get(task1.id)
                                if task1_position is None:
                                    task1_position = task1.abs_position - self._this_nodes_position
                                    self._node_abs_positions[task1.id] = task1_position

                                # Walk backwards
                                for reverse_index in range(task1_position-1, -1, -1):
                                    prev_node = self._nodes_in_region[reverse_index]
                                    # Only specific node types can write to variables.
                                    # These are Assignment, Loop, Call and CodeBlock
                                    if isinstance(prev_node, Assignment):
                                        # If it is an Assignment, check whether the
                                        # LHS reference is this Reference, which
                                        # we can do with equality check.
                                        if prev_node.lhs == index1:
                                            last_write_1 = prev_node
                                            break
                                    if isinstance(prev_node, Loop):
                                        # If it is a Loop, check whether the
                                        # loop variable is this Reference's symbol
                                        if prev_node.variable == index1.symbol:
                                            last_write_1 = prev_node
                                            break
                                    if isinstance(prev_node, CodeBlock):
                                        # Dissallow codeblocks
                                        assert False #TODO Error
                                    if isinstance(prev_node, Call):
                                        # Disallow Calls unless they are CodedKern
                                        if not isinstance(prev_node, CodedKern):
                                            assert False #TODO Error
                                        # If its a codedKern, then we will need to do
                                        # smarter analysis later, but for now
                                        # we will assume the value is a constant.
                                        last_write_1 = prev_node
                                        break


                                # If previous access was a loop variable, compute the
                                # range of starting values (or if stop_val is not a
                                # Literal, compute the first 5). If step_val is not a
                                # Literal, then compare first value and if step_val == step_val

                                # If the previous access is a write of a Reference, we'll have 
                                # to do something smart to guess.


                                # Find the prevoius access to index2
                                # Start walking backwards from this node's position
                                last_write_2 = None
                                task2_position = self._node_abs_positions.get(task2.id)
                                if task2_position is None:
                                    task2_position = task2.abs_position - self._this_nodes_position
                                    self._node_abs_positions[task2.id] = task2_position
                                
                                # Walk backwards
                                for reverse_index in range(task2_position-1, -1, -1):
                                    prev_node = self._nodes_in_region[reverse_index]
                                    # Only specific node types can write to variables.
                                    # These are Assignment, Loop, Call and CodeBlock
                                    if isinstance(prev_node, Assignment):
                                        # If it is an Assignment, check whether the
                                        # LHS reference is this Reference, which
                                        # we can do with equality check.
                                        if prev_node.lhs == index2:
                                            last_write_2 = prev_node
                                            break
                                    if isinstance(prev_node, Loop):
                                        # If it is a Loop, check whether the
                                        # loop variable is this Reference's symbol
                                        if prev_node.variable == index2.symbol:
                                            last_write_2 = prev_node
                                            break
                                    if isinstance(prev_node, CodeBlock):
                                        # Dissallow codeblocks
                                        assert False #TODO Error
                                    if isinstance(prev_node, Call):
                                        # Disallow Calls unless they are CodedKern
                                        if not isinstance(prev_node, CodedKern):
                                            assert False #TODO Error
                                        # If its a codedKern, then we will need to do
                                        # smarter analysis later, but for now
                                        # we will assume the value is a constant.
                                        last_write_2 = prev_node
                                        break

                                # If either last_write_1 or last_write_2 is None, then
                                # the last write was outside of this serial region, which
                                # means that this vairable is a constant, so both References
                                # must be to the same variable
                                if (last_write_1 is None or last_write_2 is None):
                                    if index1 != index2:
                                       assert False #TODO Error code
                                    # Otherwise this pair of indices is ok so move to the next
                                    continue

                                # Compute the (first 5) possible values of each index.
                                possible_index_1 = []
                                possible_index_2 = []

                                # For assignments, last_write_1 and last_write_2 must be the same assignment unless
                                # both are assignments to the same literal.
                                if isinstance(last_write_1, Assignment) or isinstance(last_write_2, Assignment):
                                    # If only one is an assignment then its no good.
                                    if not isinstance(last_write_1, Assignment) or not isinstance(last_write_2, Assignment):
                                        assert False # TODO
                                    if len(last_write_1.rhs.walk(Reference)) != 0 or len(last_write_2.rhs.walk(Reference)) != 0:
                                        # Need to check it is the same Assignment, not just equal as x = a and x = a are not necessarily equivalent
                                        # (a can change between)
                                        if last_write_1 is not last_write_2: 
                                            assert False # TODO
                                        # This is fine so we can move on to the next index
                                        continue
                                    else:
                                        # The rhs of both only contains Literals, so we can compute the value of the RHS and compare.
                                        # For now we will just check the RHS are equivalent (using ==) but 3 == 1+2 is something
                                        # we should check in future.
                                        if last_write_1.rhs != last_write_2.rhs:
                                            assert False # TODO
                                        # Otherwise the assignment is to the same value so we can move on to the next index
                                        continue

                                        # FIXME At some point we should compute the value of this RHS, and use it to compare to
                                        # possible values of the other index later, since the correct dependency will be found if
                                        # X is in [V,W,X,Y,Z] etc.

                                # If either is a CodedKern access then the Reference needs to be to the same
                                # variable and the CodedKern needs to be the same instance.
                                if isinstance(last_write_1, CodedKern) or isinstance(last_write_2, CodedKern):
                                    if index != index2 or last_write_1 is not last_write_2:
                                        assert False # TODO
                                    # Both indices are to the same symbol and are updated at the same CodedKern
                                    continue


                                # Left with only loops remaining, so need to do some analysis
                                
                                # If the start value of either loop is not a Literal we cannot compute it for now.
                                # FIXME: we could in principle allow BinaryOperations containing no References here as well.
                                if not isinstance(last_write_1.children[0], Literal) or not isinstance(last_write_2.children[0], Literal):
                                    assert False # TODO

                                # If the step value of either loop is not a Literal we cannot compute it for now.
                                if not isinstance(last_write.children[2], Literal) or not isinstance(last_write_3.children[2], Literal):
                                    assert False # TODO

                                # In the case that start and step are both Literals, we can compare them to check validity
                                if last_write_1.children[0] != last_write_2.children[0]:
                                    assert False # TODO

                                if last_write_2.children[2] != last_write_2.children[2]:
                                    assert False # TODO

            # For each output symbol to task 1, check whether its used in task 2
            for outvar in task1_outs.children:
                for var2 in zip(task2_ins.children, task2_outs.children):
                    if outvar.symbol == var2.symbol:
                        # Only an issue for Array[OfStructure]Reference so skip
                        # other types
                        if not isinstance(invar, ArrayMixin):
                            continue
                        # Check if bad collision. This is same check as above
                        # so I should extract it into a function.
                        # Have an array, need to compare indices to check if
                        # they are compatible.
                        for count, index1 in enumerate(outvar.indices):
                            index2 = var2.indices[count]
                            if (isinstance(index1, Literal) and 
                                index1 != index2):
                                # This condition is actually overly tight.
                                # A reference to a constant can validly
                                # be equivalent to a Literal if we know
                                # the value of that constant.
                                assert False # Throw exception
                            if isinstance(index1, Reference):
                                # For now we reject Reference vs Literal
                                if isinstance(index2, Literal):
                                    assert False # Throw exception

                                # Once we have a reference or Binary Operation
                                # we need to compute the possible values it
                                # might take.
                                
                                # Find the previous access to this
                                # Start walking backwards from this node's position
                                last_write_1 = None
                                task1_position = self._node_abs_positions.get(task1.id)
                                if task1_position is None:
                                    task1_position = task1.abs_position - self._this_nodes_position
                                    self._node_abs_positions[task1.id] = task1_position

                                # Walk backwards
                                for reverse_index in range(task1_position-1, -1, -1):
                                    prev_node = self._nodes_in_region[reverse_index]
                                    # Only specific node types can write to variables.
                                    # These are Assignment, Loop, Call and CodeBlock
                                    if isinstance(prev_node, Assignment):
                                        # If it is an Assignment, check whether the
                                        # LHS reference is this Reference, which
                                        # we can do with equality check.
                                        if prev_node.lhs == index1:
                                            last_write_1 = prev_node
                                            break
                                    if isinstance(prev_node, Loop):
                                        # If it is a Loop, check whether the
                                        # loop variable is this Reference's symbol
                                        if prev_node.variable == index1.symbol:
                                            last_write_1 = prev_node
                                            break
                                    if isinstance(prev_node, CodeBlock):
                                        # Dissallow codeblocks
                                        assert False #TODO Error
                                    if isinstance(prev_node, Call):
                                        # Disallow Calls unless they are CodedKern
                                        if not isinstance(prev_node, CodedKern):
                                            assert False #TODO Error
                                        # If its a codedKern, then we will need to do
                                        # smarter analysis later, but for now
                                        # we will assume the value is a constant.
                                        last_write_1 = prev_node
                                        break


                                # If previous access was a loop variable, compute the
                                # range of starting values (or if stop_val is not a
                                # Literal, compute the first 5). If step_val is not a
                                # Literal, then compare first value and if step_val == step_val

                                # If the previous access is a write of a Reference, we'll have 
                                # to do something smart to guess.


                                # Find the prevoius access to index2
                                # Start walking backwards from this node's position
                                last_write_2 = None
                                task2_position = self._node_abs_positions.get(task2.id)
                                if task2_position is None:
                                    task2_position = task2.abs_position - self._this_nodes_position
                                    self._node_abs_positions[task2.id] = task2_position
                                
                                # Walk backwards
                                for reverse_index in range(task2_position-1, -1, -1):
                                    prev_node = self._nodes_in_region[reverse_index]
                                    # Only specific node types can write to variables.
                                    # These are Assignment, Loop, Call and CodeBlock
                                    if isinstance(prev_node, Assignment):
                                        # If it is an Assignment, check whether the
                                        # LHS reference is this Reference, which
                                        # we can do with equality check.
                                        if prev_node.lhs == index2:
                                            last_write_2 = prev_node
                                            break
                                    if isinstance(prev_node, Loop):
                                        # If it is a Loop, check whether the
                                        # loop variable is this Reference's symbol
                                        if prev_node.variable == index2.symbol:
                                            last_write_2 = prev_node
                                            break
                                    if isinstance(prev_node, CodeBlock):
                                        # Dissallow codeblocks
                                        assert False #TODO Error
                                    if isinstance(prev_node, Call):
                                        # Disallow Calls unless they are CodedKern
                                        if not isinstance(prev_node, CodedKern):
                                            assert False #TODO Error
                                        # If its a codedKern, then we will need to do
                                        # smarter analysis later, but for now
                                        # we will assume the value is a constant.
                                        last_write_2 = prev_node
                                        break

                                # If either last_write_1 or last_write_2 is None, then
                                # the last write was outside of this serial region, which
                                # means that this vairable is a constant, so both References
                                # must be to the same variable
                                if (last_write_1 is None or last_write_2 is None):
                                    if index1 != index2:
                                       assert False #TODO Error code
                                    # Otherwise this pair of indices is ok so move to the next
                                    continue

                                # Compute the (first 5) possible values of each index.
                                possible_index_1 = []
                                possible_index_2 = []

                                # For assignments, last_write_1 and last_write_2 must be the same assignment unless
                                # both are assignments to the same literal.
                                if isinstance(last_write_1, Assignment) or isinstance(last_write_2, Assignment):
                                    # If only one is an assignment then its no good.
                                    if not isinstance(last_write_1, Assignment) or not isinstance(last_write_2, Assignment):
                                        assert False # TODO
                                    if len(last_write_1.rhs.walk(Reference)) != 0 or len(last_write_2.rhs.walk(Reference)) != 0:
                                        # Need to check it is the same Assignment, not just equal as x = a and x = a are not necessarily equivalent
                                        # (a can change between)
                                        if last_write_1 is not last_write_2: 
                                            assert False # TODO
                                        # This is fine so we can move on to the next index
                                        continue
                                    else:
                                        # The rhs of both only contains Literals, so we can compute the value of the RHS and compare.
                                        # For now we will just check the RHS are equivalent (using ==) but 3 == 1+2 is something
                                        # we should check in future.
                                        if last_write_1.rhs != last_write_2.rhs:
                                            assert False # TODO
                                        # Otherwise the assignment is to the same value so we can move on to the next index
                                        continue

                                        # FIXME At some point we should compute the value of this RHS, and use it to compare to
                                        # possible values of the other index later, since the correct dependency will be found if
                                        # X is in [V,W,X,Y,Z] etc.

                                # If either is a CodedKern access then the Reference needs to be to the same
                                # variable and the CodedKern needs to be the same instance.
                                if isinstance(last_write_1, CodedKern) or isinstance(last_write_2, CodedKern):
                                    if index != index2 or last_write_1 is not last_write_2:
                                        assert False # TODO
                                    # Both indices are to the same symbol and are updated at the same CodedKern
                                    continue


                                # Left with only loops remaining, so need to do some analysis
                                
                                # If the start value of either loop is not a Literal we cannot compute it for now.
                                # FIXME: we could in principle allow BinaryOperations containing no References here as well.
                                if not isinstance(last_write_1.children[0], Literal) or not isinstance(last_write_2.children[0], Literal):
                                    assert False # TODO

                                # If the step value of either loop is not a Literal we cannot compute it for now.
                                if not isinstance(last_write.children[2], Literal) or not isinstance(last_write_3.children[2], Literal):
                                    assert False # TODO

                                # In the case that start and step are both Literals, we can compare them to check validity
                                if last_write_1.children[0] != last_write_2.children[0]:
                                    assert False # TODO

                                if last_write_2.children[2] != last_write_2.children[2]:
                                    assert False # TODO

    # No issues detected so we carry on.


    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPSerial is not enclosed \
                                 within some OpenMP parallel region.
        :raises GenerationError: if this OMPSerial is enclosed within \
                                 any OMPSerialDirective subclass region.

        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As a Parallel Child
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        # Also check the single region is not enclosed within another OpenMP
        # single region.
        # It could in principle be allowed for that parent to be a ParallelDo
        # directive, however I can't think of a use case that would be done
        # best in a parallel code by that pattern
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                "{} must be inside an OMP parallel region but "
                "could not find an ancestor OMPParallelDirective node".format(
                    self._text_name))

        if self.ancestor(OMPSerialDirective):
            raise GenerationError(
                    "{} must not be inside another OpenMP "
                    "serial region".format(self._text_name))

        self._validate_satisfiable_task_dependencies()

        super(OMPSerialDirective, self).validate_global_constraints()


class OMPSingleDirective(OMPSerialDirective):
    '''
    Class representing an OpenMP SINGLE directive in the PSyIR.

    :param list children: List of Nodes that are children of this Node.
    :param parent: The Node in the AST that has this directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param bool nowait: Argument describing whether this single should have \
        a nowait clause applied. Default value is False.

    '''
    _children_valid_format = "Schedule, [OMPNowaitClause]"
    # Textual description of the node
    _text_name = "OMPSingleDirective"

    def __init__(self, children=None, parent=None, nowait=False):

        self._nowait = nowait
        # Call the init method of the base class once we've stored
        # the nowait requirement
        super(OMPSingleDirective, self).__init__(children=children,
                                                 parent=parent)
        if self._nowait:
            self.children.append(OMPNowaitClause())

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         The rules are:
         1. Child 0 must always be a Schedule.
         2. Child 1 can only be a OMPNowaitClause.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Schedule)
        if position == 1:
            return isinstance(child, OMPNowaitClause)
        return False

    @property
    def nowait(self):
        '''
        :returns: whether the nowait clause is specified for this directive.
        :rtype: bool

        '''
        return self._nowait

    def gen_code(self, parent):
        '''Generate the fortran OMP Single Directive and any associated
        code

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        # Check the constraints are correct
        self.validate_global_constraints()

        # Capture the nowait section of the string if required
        nowait_string = ""
        if self._nowait:
            nowait_string = "nowait"

        parent.add(DirectiveGen(parent, "omp", "begin", "single",
                                nowait_string))

        # Generate the code for all of this node's children
        for child in self.dir_body:
            child.gen_code(parent)

        # Generate the end code for this node
        parent.add(DirectiveGen(parent, "omp", "end", "single", ""))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp single". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp single"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end single". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end single"


class OMPMasterDirective(OMPSerialDirective):
    '''
    Class representing an OpenMP MASTER directive in the PSyclone AST.

    '''

    # Textual description of the node
    _text_name = "OMPMasterDirective"

    def gen_code(self, parent):
        '''Generate the Fortran OMP Master Directive and any associated
        code

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''

        # Check the constraints are correct
        self.validate_global_constraints()

        parent.add(DirectiveGen(parent, "omp", "begin", "master", ""))

        # Generate the code for all of this node's children
        for child in self.children:
            child.gen_code(parent)

        # Generate the end code for this node
        parent.add(DirectiveGen(parent, "omp", "end", "master", ""))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp master". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp master"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end master". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end master"


class OMPParallelDirective(OMPRegionDirective):
    ''' Class representing an OpenMP Parallel directive. '''

    _children_valid_format = ("Schedule, OMPDefaultClause, [OMPPrivateClause],"
                              " [OMPReductionClause]*")

    def __init__(self, children=None, parent=None):
        super(OMPParallelDirective, self).__init__(children=children,
                                                   parent=parent)
        self.addchild(OMPDefaultClause(clause_type=OMPDefaultClause.
                                       DefaultClauseTypes.SHARED))

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0 and isinstance(child, Schedule):
            return True
        if position == 1 and isinstance(child, OMPDefaultClause):
            return True
        if position == 2 and isinstance(child, (OMPPrivateClause,
                                                OMPReductionClause)):
            return True
        if position >= 3 and isinstance(child, OMPReductionClause):
            return True
        return False

    def gen_code(self, parent):
        '''Generate the fortran OMP Parallel Directive and any associated
        code'''
        from psyclone.psyGen import zero_reduction_variables

        private_clause = self._get_private_clause()

        reprod_red_call_list = self.reductions(reprod=True)
        if reprod_red_call_list:
            # we will use a private thread index variable
            thread_idx = self.scope.symbol_table.\
                lookup_with_tag("omp_thread_index")
            private_clause.addchild(Reference(thread_idx))
            thread_idx = thread_idx.name
            # declare the variable
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[thread_idx]))
        if len(self._children) >= 3:
            if private_clause != self._children[2]:
                if isinstance(self._children[2], OMPPrivateClause):
                    self._children[2] = private_clause
                else:
                    self.addchild(private_clause, index=2)
        else:
            self.addchild(private_clause, index=2)

        # We're not doing nested parallelism so make sure that this
        # omp parallel region is not already within some parallel region
        self.validate_global_constraints()

        # Check that this OpenMP PARALLEL directive encloses other
        # OpenMP directives. Although it is valid OpenMP if it doesn't,
        # this almost certainly indicates a user error.
        self._encloses_omp_directive()

        calls = self.reductions()

        # first check whether we have more than one reduction with the same
        # name in this Schedule. If so, raise an error as this is not
        # supported for a parallel region.
        names = []
        for call in calls:
            name = call.reduction_arg.name
            if name in names:
                raise GenerationError(
                    f"Reduction variables can only be used once in an invoke. "
                    f"'{name}' is used multiple times, please use a different "
                    f"reduction variable")
            names.append(name)

        zero_reduction_variables(calls, parent)

        default_str = self.children[1]._clause_string
        private_list = []
        for child in self.children[2].children:
            private_list.append(child.symbol.name)
        private_str = "private(" + ",".join(private_list) + ")"
        parent.add(DirectiveGen(parent, "omp", "begin", "parallel",
                                f"{default_str}, {private_str}"))

        if reprod_red_call_list:
            # add in a local thread index
            parent.add(UseGen(parent, name="omp_lib", only=True,
                              funcnames=["omp_get_thread_num"]))
            parent.add(AssignGen(parent, lhs=thread_idx,
                                 rhs="omp_get_thread_num()+1"))

        first_type = type(self.dir_body[0])
        for child in self.dir_body.children:
            if first_type != type(child):
                raise NotImplementedError("Cannot correctly generate code"
                                          " for an OpenMP parallel region"
                                          " containing children of "
                                          "different types")
            child.gen_code(parent)

        parent.add(DirectiveGen(parent, "omp", "end", "parallel", ""))

        if reprod_red_call_list:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " sum the partial results "
                                  "sequentially"))
            parent.add(CommentGen(parent, ""))
            for call in reprod_red_call_list:
                call.reduction_sum_loop(parent)

        self.gen_post_region_code(parent)

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp parallel". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        result = "omp parallel"
        # TODO #514: not yet working with NEMO, so commented out for now
        # if not self._reprod:
        #     result += self._reduction_string()
        private_clause = self._get_private_clause()
        if len(self._children) >= 3:
            if private_clause != self._children[2]:
                if isinstance(self._children[2], OMPPrivateClause):
                    self._children[2] = private_clause
                else:
                    self.addchild(private_clause, index=2)
        else:
            self.addchild(private_clause, index=2)

        return result

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end parallel". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end parallel"

    def _get_private_clause(self):
        '''
        Returns the variable names used for any loops within a directive
        and any variables that have been declared private by a Kernel
        within the directive.

        :returns: A private clause containing the variables that need to be
                  private for this directive.
        :rtype: :py:class:`psyclone.psyir.nodes.omp_clauses.OMPPrivateClause`

        :raises InternalError: if a Kernel has local variable(s) but they \
                               aren't named.
        '''
        from psyclone.psyGen import InvokeSchedule
        result = set()
        # get variable names from all calls that are a child of this node
        for call in self.kernels():
            for variable_name in call.local_vars():
                if variable_name == "":
                    raise InternalError(
                        f"call '{call.name}' has a local variable but its "
                        f"name is not set.")
                result.add(variable_name.lower())

        # Now determine scalar variables that must be private:
        var_accesses = VariablesAccessInfo()
        self.reference_accesses(var_accesses)
        for signature in var_accesses.all_signatures:
            accesses = var_accesses[signature].all_accesses
            # Ignore variables that have indices, we only look at scalar
            if accesses[0].is_array():
                continue

            # If a variable is only accessed once, it is either an error
            # or a shared variable - anyway it is not private
            if len(accesses) == 1:
                continue

            # We have at least two accesses. If the first one is a write,
            # assume the variable should be private:
            if accesses[0].access_type == AccessType.WRITE:
                # Check if the write access is inside the parallel loop. If
                # the write is outside of a loop, it is an assignment to
                # a shared variable. Example where jpk is likely used
                # outside of the parallel section later, so it must be
                # declared as shared in order to have its value in other loops:
                # !$omp parallel
                # jpk = 100
                # !omp do
                # do ji = 1, jpk

                # TODO #598: improve the handling of scalar variables.

                # Go up the tree till we either find the InvokeSchedule,
                # which is at the top, or a Loop statement (or no parent,
                # which means we have reached the end of a called kernel).
                parent = accesses[0].node.ancestor((Loop, InvokeSchedule),
                                                   include_self=True)

                if parent and isinstance(parent, Loop):
                    # The assignment to the variable is inside a loop, so
                    # declare it to be private
                    result.add(str(signature).lower())

        # Convert the set into a list and sort it, so that we get
        # reproducible results
        list_result = list(result)
        list_result.sort()

        # Create the OMPPrivateClause corresponding to the results
        priv_clause = OMPPrivateClause()
        symbol_table = self.scope.symbol_table
        for ref_name in list_result:
            symbol = symbol_table.lookup(ref_name)
            ref = Reference(symbol)
            priv_clause.addchild(ref)
        return priv_clause

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPDoDirective is not enclosed \
                            within some OpenMP parallel region.
        '''
        if self.ancestor(OMPParallelDirective) is not None:
            raise GenerationError("Cannot nest OpenMP parallel regions.")

    def _encloses_omp_directive(self):
        ''' Check that this Parallel region contains other OpenMP
            directives. While it doesn't have to (in order to be valid
            OpenMP), it is likely that an absence of directives
            is an error on the part of the user. '''
        # We need to recurse down through all our children and check
        # whether any of them are an OMPRegionDirective.
        node_list = self.walk(OMPRegionDirective)
        if not node_list:
            # TODO raise a warning here so that the user can decide
            # whether or not this is OK.
            pass
            # raise GenerationError("OpenMP parallel region does not enclose "
            #                       "any OpenMP directives. This is probably "
            #                       "not what you want.")


class OMPTaskDirective(OMPRegionDirective):
    '''
    Class representing an OpenMP TASK directive in the PSyIR.

    :param list children: list of Nodes that are children of this Node.
    :param parent: the Node in the AST that has this directive as a child
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    '''
    _children_valid_format = ("Schedule, OMPPrivateClause,"
                              "OMPFirstprivateClause, OMPSharedClause"
                              "OMPDependClause, OMPDependClause")

    def __init__(self, children=None, parent=None):
        super(OMPTaskDirective, self).__init__(children=children,
                                               parent=parent)
        # We don't know if we have a parent OMPParallelClause at initialisation
        # so we can only create dummy clauses for now.
        self.children.append(OMPPrivateClause())
        self.children.append(OMPFirstprivateClause())
        self.children.append(OMPSharedClause())
        self.children.append(OMPDependClause(
                depend_type=OMPDependClause.DependClauseTypes.IN))
        self.children.append(OMPDependClause(
                depend_type=OMPDependClause.DependClauseTypes.OUT))
        # We store the symbol names for the parent loops so we can work out the
        # "chunked" loop variables.
        self._parent_loop_vars = []
        self._parent_loops = []
        self._proxy_loop_vars = {}
        self._parent_parallel = None
        self._parallel_private = None

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         The rules are:
         1. Child 0 must always be a Schedule.
         2. Child 1 must always be an OMPPrivateClause
         3. Child 2 must always be an OMPFirstprivateClause
         4. Child 3 must always be an OMPSharedClause
         5. Child 4 and 5 must always be OMPDependClauses

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Schedule)
        if position == 1:
            return isinstance(child, OMPPrivateClause)
        if position == 2:
            return isinstance(child, OMPFirstprivateClause)
        if position == 3:
            return isinstance(child, OMPSharedClause)
        if position in (4, 5):
            return isinstance(child, OMPDependClause)
        return False

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPTaskDirective is not \
                                 enclosed within an OpenMP serial region.
        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). A taskloop
        # directive, we must have an OMPSerialDirective as an
        # ancestor back up the tree.
        if not self.ancestor(OMPSerialDirective):
            raise GenerationError(
                "OMPTaskDirective must be inside an OMP Serial region "
                "but could not find an ancestor node.")

    def _find_parent_loop_vars(self):
        '''
        Finds the loop variable of each parent loop inside the same
        OMPParallelDirective and stores them in the _parent_loop_vars member.
        Also stores the parent OMPParallelDirective in _parent_parallel.
        '''
        anc = self.ancestor((OMPParallelDirective, Loop))
        while isinstance(anc, Loop):
            # Store the loop variable of the parent loop
            var = anc.variable
            self._parent_loop_vars.append(var)
            self._parent_loops.append(anc)
            # Recurse up the tree
            anc = anc.ancestor((OMPParallelDirective, Loop))

        # Store the parent parallel directive node
        self._parent_parallel = anc
        self._parallel_private = anc._get_private_clause().children

    def _evaluate_readonly_baseref(self, ref, private_list, firstprivate_list,
                                   in_list):
        '''
        Evaluates any read-only References to variables inside the OpenMP task
        region and adds a copy of the Reference to the appropriate data-sharing
        list used to construct the clauses for this task region.

        The basic rules for this are:
        1. If the Reference is private in the parallel region containing this
        task, the Reference will be added to the list of firstprivate
        References unless it has already been added to either the list of
        private or firstprivate References for this task.
        2. If the Reference is shared, then the Reference will be added to the
        input list of References unless it is already present in that list.

        :param ref: The reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References for this task.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References for this
                                  task.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        '''
        is_private = (ref in self._parallel_private)
        if is_private:
            # If the reference is private in the parent parallel,
            # then it is added to the firstprivate clause for this
            # task if it has not yet been written to (i.e. is not
            # yet in the private clause list).
            if ref not in private_list and ref not in firstprivate_list:
                firstprivate_list.append(ref.copy())
        else:
            # Otherwise it was a shared variable. Its not an
            # array so we just add the name to the in_list
            # if not already there. If its already in out_list
            # we still add it as this is the same as an inout
            # dependency
            if ref not in in_list:
                in_list.append(ref.copy())

    def _handle_index_binop(self, node, index_list,
                            firstprivate_list, private_list):
        '''
        Evaluates a binary operation index used to access an array
        within this OpenMP task.

        For each index, the code checks that the index matches the expected
        format, which is [Reference] [ADD/SUB] [Literal] (or the opposite
        ordering). PSyclone does not currently support other binary operation
        indexing inside an OpenMP task.

        Once this is confirmed, PSyclone builds the appropriate list of
        References to correctly express the dependencies of this array access,
        and appends them to the `index_list` input argument. This can depend on
        the structure of the Loop inside the task, and any parent Loops.

        The Reference inside the binary operation must be a private or
        firstprivate variable inside the task region, else PSyclone does not
        support using it as an array index.

        :param node: The BinaryOperation to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`
        :param index_list: A list of Nodes used to handle the dependencies
                           for this array access. This may be reused over
                           multiple calls to this function to avoid duplicating
                           Nodes.
        :type index_list: List of :py:class:`psyclone.psyir.nodes.Node`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in
                             this task region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: if this BinaryOperation is not an addition or
                                 subtraction.
        :raises GenerationError: if this BinaryOperation does not contain both
                                 a Reference and a Literal.
        :raises GenerationError: if this BinaryOperation contains a Reference
                                 to a shared variable.
        '''

        # Binary Operation check
        if node.operator is not \
           BinaryOperation.Operator.ADD and \
           node.operator is not \
           BinaryOperation.Operator.SUB:
            raise GenerationError(
                f"Binary Operator of type {node.operator} used "
                "as in index inside an "
                "OMPTaskDirective which is not "
                "supported")
        # We have ADD or SUB BinaryOperation
        # It must be either Ref OP Lit or Lit OP Ref
        if not((isinstance(node.children[0], Reference)
                and isinstance(node.children[1], Literal))
                or (isinstance(node.children[0], Literal)
                    and isinstance(node.children[1], Reference))):
            raise GenerationError(
                "Children of BinaryOperation are of "
                f"types {type(node.children[0]).__name__} and "
                f"{type(node.children[1]).__name__}, expected one "
                "Reference and one Literal when"
                " used as an index inside an "
                "OMPTaskDirective.")

        # Have Reference +/- Literal, analyse
        # and create clause appropriately
        index_private = False
        is_proxy = False
        ref = None
        literal = None
        ref_index = None
        if isinstance(node.children[0], Reference):
            index_symbol = node.children[0].symbol
            index_private = (node.children[0] in self._parallel_private)
            is_proxy = (index_symbol in self._proxy_loop_vars)
            ref = node.children[0]
            ref_index = 0
            literal = node.children[1]
        if isinstance(node.children[1], Reference):
            index_symbol = node.children[1].symbol
            index_private = (node.children[1] in self._parallel_private)
            is_proxy = (index_symbol in self._proxy_loop_vars)
            ref = node.children[1]
            ref_index = 1
            literal = node.children[0]

        # We have some array access which is of the format:
        # array( Reference +/- Literal).
        # If the Reference is to a proxy (is_proxy is True) then we replace
        # the Reference with the proxy variable instead. This is the most
        # important case.
        # If the task contains Loops, and the Reference is to one of the
        # Loop variables, then we create a Range object for : for that
        # dimension. All other situations are treated as a constant.

        # Find the child loops that are not proxies.
        child_loop_vars = []
        for child_loop in self.walk(Loop):
            if child_loop.variable not in self._proxy_loop_vars:
                child_loop_vars.append(child_loop.variable)

        # Handle the proxy_loop case
        if is_proxy:
            # Treat it as though we came across the parent loop variable.
            parent_loop = self._proxy_loop_vars[index_symbol]['parent_loop']
            real_var = self._proxy_loop_vars[index_symbol]['parent_var']

            # Create a Reference to the real variable
            real_ref = Reference(real_var)
            # We have a Literal step value, and a Literal in
            # the Binary Operation. These Literals must both be
            # Integer types, so we will convert them to integers
            # and do some divison.
            step_val = int(parent_loop.step_expr.value)
            literal_val = int(literal.value)
            divisor = math.ceil(literal_val / step_val)
            modulo = literal_val % step_val
            # If the divisor is > 1, then we need to do
            # divisor*step_val
            # We also need to add divisor-1*step_val to cover the case
            # where e.g. array(i+1) is inside a larger loop, as we
            # need dependencies to array(i) and array(i+step), unless
            # modulo == 0
            step = None
            step2 = None
            if divisor > 1:
                step = BinaryOperation.create(
                        BinaryOperation.Operator.MUL,
                        Literal(f"{divisor}", INTEGER_TYPE),
                        Literal(f"{step_val}", INTEGER_TYPE))
                if divisor > 2:
                    step2 = BinaryOperation.create(
                            BinaryOperation.Operator.MUL,
                            Literal(f"{divisor-1}", INTEGER_TYPE),
                            Literal(f"{step_val}", INTEGER_TYPE))
                else:
                    step2 = Literal(f"{step_val}", INTEGER_TYPE)
            else:
                step = Literal(f"{step_val}", INTEGER_TYPE)

            # Create a Binary Operation of the correct format.
            binop = None
            binop2 = None
            if ref_index == 0:
                # We have Ref OP Literal
                binop = BinaryOperation.create(
                        node.operator, real_ref.copy(), step)
                if modulo != 0:
                    if step2 is not None:
                        binop2 = BinaryOperation.create(
                                 node.operator, real_ref.copy(), step2)
                    else:
                        binop2 = real_ref.copy()
            else:
                # We have Literal OP Ref
                binop = BinaryOperation.create(
                        node.operator, step, real_ref.copy())
                if modulo != 0:
                    if step2 is not None:
                        binop2 = BinaryOperation.create(
                                 node.operator, step2, real_ref.copy())
                    else:
                        binop2 = real_ref.copy()
            # Add this to the list of indexes
            if binop2 is not None:
                index_list.append([binop, binop2])
            else:
                index_list.append(binop)

        # Proxy use case handled.
        # If the variable is private:
        elif index_private:
            # If the variable is in our private list
            if ref in private_list:
                # If its a child loop variable
                if ref.symbol in child_loop_vars:
                    # Return a full range (:)
                    dim = len(index_list)
                    one = Literal(str(dim+1), INTEGER_TYPE)
                    # Find the arrayref
                    arrayref = ref.parent.parent
                    lbound = BinaryOperation.create(
                            BinaryOperation.Operator.LBOUND,
                            Reference(arrayref.symbol), one.copy())
                    ubound = BinaryOperation.create(
                            BinaryOperation.Operator.UBOUND,
                            Reference(arrayref.symbol), one.copy())
                    full_range = Range.create(lbound, ubound)
                    index_list.append(full_range)
                else:
                    # We have a private constant, written to inside
                    # our region, so we can't do anything better than
                    # a full range I think (since we don't know what
                    # the value is/how it changes.
                    # Return a full range (:)
                    dim = len(index_list)
                    one = Literal(str(dim+1), INTEGER_TYPE)
                    arrayref = ref.parent.parent
                    lbound = BinaryOperation.create(
                            BinaryOperation.Operator.LBOUND,
                            Reference(arrayref.symbol), one.copy())
                    ubound = BinaryOperation.create(
                            BinaryOperation.Operator.UBOUND,
                            Reference(arrayref.symbol), one.copy())
                    full_range = Range.create(lbound, ubound)
                    index_list.append(full_range)
            else:
                if ref not in firstprivate_list:
                    firstprivate_list.append(ref.copy())
                if ref.symbol in self._parent_loop_vars:
                    # Non-proxy access to a parent loop variable.
                    # In this case we have to do similar to when accessing a
                    # proxy loop variable.

                    # Find index of parent loop var
                    ind = self._parent_loop_vars.index(ref.symbol)
                    parent_loop = self._parent_loops[ind]
                    # We have a Literal step value, and a Literal in
                    # the Binary Operation. These Literals must both be
                    # Integer types, so we will convert them to integers
                    # and do some divison.
                    step_val = int(parent_loop.step_expr.value)
                    literal_val = int(literal.value)
                    divisor = math.ceil(literal_val / step_val)
                    modulo = literal_val % step_val
                    # If the divisor is > 1, then we need to do
                    # divisor*step_val
                    # We also need to add divisor-1*step_val to cover the case
                    # where e.g. array(i+1) is inside a larger loop, as we
                    # need dependencies to array(i) and array(i+step), unless
                    # modulo == 0
                    step = None
                    step2 = None
                    if divisor > 1:
                        step = BinaryOperation.create(
                                BinaryOperation.Operator.MUL,
                                Literal(f"{divisor}", INTEGER_TYPE),
                                Literal(f"{step_val}", INTEGER_TYPE))
                        if divisor > 2:
                            step2 = BinaryOperation.create(
                                    BinaryOperation.Operator.MUL,
                                    Literal(f"{divisor-1}", INTEGER_TYPE),
                                    Literal(f"{step_val}", INTEGER_TYPE))
                        else:
                            step2 = Literal(f"{step_val}", INTEGER_TYPE)
                    else:
                        step = Literal(f"{step_val}", INTEGER_TYPE)

                    # Create a Binary Operation of the correct format.
                    binop = None
                    binop2 = None
                    if ref_index == 0:
                        # We have Ref OP Literal
                        binop = BinaryOperation.create(
                                node.operator, ref.copy(), step)
                        if modulo != 0:
                            if step2 is not None:
                                binop2 = BinaryOperation.create(
                                         node.operator, ref.copy(), step2)
                            else:
                                binop2 = ref.copy()
                    else:
                        # We have Literal OP Ref
                        binop = BinaryOperation.create(
                                node.operator, step, ref.copy())
                        if modulo != 0:
                            if step2 is not None:
                                binop2 = BinaryOperation.create(
                                         node.operator, step2, ref.copy())
                            else:
                                binop2 = ref.copy()
                    # Add this to the list of indexes
                    if binop2 is not None:
                        index_list.append([binop, binop2])
                    else:
                        index_list.append(binop)
                else:
                    # It can't be a child loop variable (these have to be
                    # private). Just has to be a firstprivate constant, which
                    # we can just use the reference to for now. Not 100% on
                    # this as the value is modifiable.
                    index_list.append(node.copy())
        else:
            # Have a shared variable, which we're not currently supporting
            raise GenerationError(
                    "Shared variable access used "
                    "as an index inside an "
                    "OMPTaskDirective which is not "
                    "supported.")

    def _evaluate_readonly_arrayref(self, ref, private_list, firstprivate_list,
                                    shared_list, in_list):
        '''
        Evaluates a read-only access to an Array inside the task region, and
        computes any data-sharing clauses and dependency clauses based upon the
        access.

        This is done by evaluating each of the array indices, and determining
        whether they are:
        1. A Literal index, in which case we need a dependency to that
           specific section of the array.
        2. A Reference index, in which case we need a dependency to the section
           of the array represented by that Reference.
        3. A Binary Operation, in which case the code calls
          `_handle_index_binop` to evaluate any additional dependencies.

        Once these have been computed, any new dependencies are added into the
        in_list, and the array reference itself will be added to the
        shared_list if not already present.

        :param node: The Reference to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: If an array index is a shared variable.
        :raises GenerationError: If an array index is not a Reference, Literal
                                 or BinaryOperation.
        '''
        index_list = []

        # Arrays are always shared variables in the parent parallel region.

        # The reference is shared. Since it is an array,
        # we need to check the following restrictions:
        # 1. No ArrayReference or ArrayOfStructuresReference
        # or StructureReference appear in the indexing.
        # 2. Each index is a firstprivate variable, or a
        # private parent variable that has not yet been
        # declared (in which case we declare it as
        # firstprivate). Alternatively each index is
        # a BinaryOperation whose children are a
        # Reference to a firstprivate variable and a
        # Literal, with operator of ADD or SUB
        for dim, index in enumerate(ref.indices):
            # pylint: disable=unidiomatic-typecheck
            if type(index) is Reference:
                # Check whether the Reference is private
                index_private = (index in
                                 self._parallel_private)
                # Check whether the reference is to a child loop variable.
                child_loop_vars = []
                for child_loop in self.walk(Loop):
                    if child_loop.variable not in self._proxy_loop_vars:
                        child_loop_vars.append(child_loop.variable)

                if index_private:
                    if (index not in private_list and
                            index not in firstprivate_list):
                        firstprivate_list.append(index.copy())
                    # Special case 1. If index belongs to a child loop
                    # that is NOT a proxy for a parent loop, then we
                    # can only do as well as guessing the entire range
                    # of the loop is used.
                    if index.symbol in child_loop_vars:
                        # Return a :
                        one = Literal(str(dim+1), INTEGER_TYPE)
                        lbound = BinaryOperation.create(
                                BinaryOperation.Operator.LBOUND,
                                Reference(ref.symbol), one.copy())
                        ubound = BinaryOperation.create(
                                BinaryOperation.Operator.UBOUND,
                                Reference(ref.symbol), one.copy())
                        full_range = Range.create(lbound, ubound)
                        index_list.append(full_range)
                    elif index.symbol in self._proxy_loop_vars:
                        # Special case 2. the index is a proxy for a parent
                        # loop's variable. In this case, we add a reference to
                        # the parent loop's value.
                        parent_var =\
                            self._proxy_loop_vars[index.symbol]['parent_var']
                        parent_ref = Reference(parent_var)
                        index_list.append(parent_ref)
                    else:
                        # Final case is just a generic Reference, in which case
                        # just copy the Reference
                        index_list.append(index.copy())
                else:
                    raise GenerationError(
                            "Shared variable access used "
                            "as an index inside an "
                            "OMPTaskDirective which is not "
                            f"supported. Variable name is {index}")
            elif isinstance(index, BinaryOperation):
                # Binary Operation check
                # A single binary operation, e.g. a(i+1) can require
                # multiple clauses to correctly handle.
                self._handle_index_binop(index, index_list,
                                         firstprivate_list,
                                         private_list)
            elif isinstance(index, Literal):
                # Just place literal directly into the dependency clause.
                index_list.append(index.copy())
            else:
                # Not allowed type appears
                raise GenerationError(
                        f"{type(index).__name__} object is not allowed to "
                        "appear in an Array Index "
                        "expression inside an "
                        "OMPTaskDirective.")
        # So we have a list of (lists of) indices
        # [ [index1, index4], index2, index3] so convert these
        # to an ArrayReference again.
        # To create all combinations, we use itertools.product
        # We have to create a new list which only contains lists.
        # Add to in_list: name(index1, index2)
        new_index_list = []
        for element in index_list:
            if isinstance(element, list):
                new_index_list.append(element)
            else:
                new_index_list.append([element])
        combinations = itertools.product(*new_index_list)
        for temp_list in combinations:
            # We need to make copies of the members as each
            # member can only be the child of one ArrayRef
            final_list = []
            for element in temp_list:
                final_list.append(element.copy())
            dclause = ArrayReference.create(ref.symbol,
                                            list(final_list))
            # Add dclause into the in_list if required
            if dclause not in in_list:
                in_list.append(dclause)
        # Add to shared_list (for explicity)
        sclause = Reference(ref.symbol)
        if sclause not in shared_list:
            shared_list.append(sclause)

    def _evaluate_readonly_reference(self, ref, private_list,
                                     firstprivate_list, shared_list, in_list):
        '''
        Evaluates any Reference used in a read context. This is done by
        calling the appropriate helper functions for ArrayReferences,
        StructureReferences or other References as appropriate.

        :param node: The Reference to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        '''
        if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
            # Resolve ArrayReference (AOSReference)
            self._evaluate_readonly_arrayref(ref, private_list,
                                             firstprivate_list, shared_list,
                                             in_list)
        elif isinstance(ref, StructureReference):
            # This is treated the same as a Reference, except we have to
            # create a Reference to the symbol to handle.
            base_ref = Reference(ref.symbol)
            self._evaluate_readonly_baseref(base_ref, private_list,
                                            firstprivate_list,
                                            in_list)
        elif isinstance(ref, Reference):
            self._evaluate_readonly_baseref(ref, private_list,
                                            firstprivate_list,
                                            in_list)

    def _evaluate_write_arrayref(self, ref, private_list, firstprivate_list,
                                 shared_list, out_list):
        '''
        Evaluates a write access to an Array inside the task region, and
        computes any data-sharing clauses and dependency clauses based upon the
        access.

        This is done by evaluating each of the array indices, and determining
        whether they are:
        1. A Literal index, in which case we need a dependency to that
           specific section of the array.
        2. A Reference index, in which case we need a dependency to the section
           of the array represented by that Reference.
        3. A Binary Operation, in which case the code calls
          `_handle_index_binop` to evaluate any additional dependencies.

        Once these have been computed, any new dependencies are added into the
        out_list, and the array reference itself will be added to the
        shared_list if not already present.

        :param ref: The Reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: If an array index is a shared variable.
        '''
        # We write to this arrayref, so its shared and depend out on
        # the array.

        # Arrays are always shared at the moment, so we ignore the possibility
        # of it being private now.

        index_list = []
        # Work out the indices needed.
        for dim, index in enumerate(ref.indices):
            if isinstance(index, Literal):
                # Literals are just a value, just use the value.
                index_list.append(index.copy())
            elif isinstance(index, Reference):
                index_private = (index in
                                 self._parallel_private)
                # Check whether the reference is to a child loop variable.
                child_loop_vars = []
                for child_loop in self.walk(Loop):
                    if child_loop.variable not in self._proxy_loop_vars:
                        child_loop_vars.append(child_loop.variable)

                if index_private:
                    if (index not in private_list and
                            index not in firstprivate_list):
                        firstprivate_list.append(index.copy())
                    # Special case 1. If index belongs to a child loop
                    # that is NOT a proxy for a parent loop, then we
                    # can only do as well as guessing the entire range
                    # of the loop is used.
                    if index.symbol in child_loop_vars:
                        # Return a :
                        one = Literal(str(dim+1), INTEGER_TYPE)
                        lbound = BinaryOperation.create(
                                BinaryOperation.Operator.LBOUND,
                                Reference(ref.symbol), one.copy())
                        ubound = BinaryOperation.create(
                                BinaryOperation.Operator.UBOUND,
                                Reference(ref.symbol), one.copy())
                        full_range = Range.create(lbound, ubound)
                        index_list.append(full_range)
                    elif index.symbol in self._proxy_loop_vars:
                        # Special case 2. the index is a proxy for a parent
                        # loop's variable. In this case, we add a reference to
                        # the parent loop's value.
                        parent_var =\
                            self._proxy_loop_vars[index.symbol]['parent_var']
                        parent_ref = Reference(parent_var)
                        index_list.append(parent_ref)
                    else:
                        # Final case is just a generic Reference, in which case
                        # just copy the Reference
                        index_list.append(index.copy())
                else:
                    raise GenerationError(
                            "Shared variable access used "
                            "as an index inside an "
                            "OMPTaskDirective which is not "
                            f"supported. Variable name is {index}")
            elif isinstance(index, BinaryOperation):
                self._handle_index_binop(index, index_list,
                                         firstprivate_list,
                                         private_list)

        # So we have a list of (lists of) indices
        # [ [index1, index4], index2, index3] so convert these
        # to an ArrayReference again.
        # To create all combinations, we use itertools.product
        # We have to create a new list which only contains lists.
        # Add to in_list: name(index1, index2)
        new_index_list = []
        for element in index_list:
            if isinstance(element, list):
                new_index_list.append(element)
            else:
                new_index_list.append([element])
        combinations = itertools.product(*new_index_list)
        for temp_list in combinations:
            # We need to make copies of the members as each
            # member can only be the child of one ArrayRef
            final_list = []
            for element in temp_list:
                final_list.append(element.copy())
            dclause = ArrayReference.create(ref.symbol,
                                            list(final_list))
            # Add dclause into the out_list if required
            if dclause not in out_list:
                out_list.append(dclause)
        # Add to shared_list (for explicity)
        sclause = Reference(ref.symbol)
        if sclause not in shared_list:
            shared_list.append(sclause)

    def _evaluate_write_baseref(self, ref, private_list,
                                shared_list, out_list):
        '''
        Evaluates a write to a non-ArrayReference reference. If the variable
        is declared private in the parent parallel region, then the variable
        is added to the private clause for this task.

        If the variable is not private (therefore is shared), it is added to
        the shared and output dependence lists for this task region.

        :param ref: The Reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        '''
        # Check if its a private variable
        is_private = ref in self._parallel_private
        # If its private should add it to private list if not already present
        if is_private and ref not in private_list:
            private_list.append(ref.copy())
        # Otherwise its shared
        if not is_private:
            if ref not in shared_list:
                shared_list.append(ref.copy())
            if ref not in out_list:
                out_list.append(ref.copy())

    def _evaluate_write_reference(self, ref, private_list, firstprivate_list,
                                  shared_list, out_list):
        '''
        Evaluates a write to any Reference in the task region. This is done by
        calling the appropriate subfunction depending on the type of the
        Reference.

        :param ref: The Reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        '''
        if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
            # Resoolve ArrayReference (AOSRef)
            self._evaluate_write_arrayref(ref, private_list, firstprivate_list,
                                          shared_list, out_list)
        elif isinstance(ref, StructureReference):
            # This is treated the same as a Reference, but we create a
            # Reference to the symbol to handle.
            base_ref = Reference(ref.symbol)
            self._evaluate_write_baseref(base_ref, private_list,
                                         shared_list, out_list)
        elif isinstance(ref, Reference):
            self._evaluate_write_baseref(ref, private_list,
                                         shared_list, out_list)

    def _evaluate_assignment(self, node, private_list, firstprivate_list,
                             shared_list, in_list, out_list):
        '''
        Evaluates an Assignment node within this task region. This is done
        by calling the appropriate subfunction on each reference on the
        LHS and RHS of the Assignment.

        :param ref: The Assignment to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Assignment`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        '''
        lhs = node.children[0]
        rhs = node.children[1]
        # Evaluate LHS
        self._evaluate_write_reference(lhs, private_list, firstprivate_list,
                                       shared_list, out_list)

        # Evaluate RHS
        references = rhs.walk(Reference)
        for ref in references:
            self._evaluate_readonly_reference(ref, private_list,
                                              firstprivate_list, shared_list,
                                              in_list)

    def _evaluate_loop(self, node, private_list, firstprivate_list,
                       shared_list, in_list, out_list):
        '''
        Evaluates a Loop node within this task Region. This is done in several
        steps:
        1. Check the loop variable, start/stop/step values, and ensure that
           they are valid. The loop variable must not be shared and the start,
           stop, and step variables are not ArrayReferences. It also detects if
           the loop variable is a "proxy", i.e. it represents a parent loop's
           variable as a chunked loop variable. Any variables that are not yet
           private, firstprivate or shared will be declared as firstprivate (as
           they are read before being accessed elsewhere).
        2. Loop through each of the nodes in the Loop's Schedule child, and
           evaluate them through the _evaluate_node call.

        :param node: The Loop to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: If the loop variable is a shared variable.
        :raises GenerationError: If the loop start, stop or step expression
                                 contains an ArrayReference.
        '''
        # Look at loop bounds etc first.
        # Find our loop initialisation, variable and bounds
        loop_var = node.variable
        start_val = node.start_expr
        stop_val = node.stop_expr
        step_val = node.step_expr

        to_remove = None

        # Check if we have a loop of type do ii = i where i is a parent loop
        # variable.
        start_val_refs = start_val.walk(Reference)
        if (len(start_val_refs) == 1 and 
                isinstance(start_val_refs[0], Reference)):
            # Loop through the parent loop variables
            for index, parent_var in enumerate(self._parent_loop_vars):
                # If its a parent loop variable, we need to make it a proxy
                # variable for now.
                if start_val_refs[0].symbol == parent_var:
                    to_remove = loop_var
                    # Store the loop and parent_var
                    subdict = {}
                    subdict['parent_var'] = parent_var
                    subdict['loop'] = node
                    subdict['parent_loop'] = self._parent_loops[index]

                    self._proxy_loop_vars[to_remove] = subdict
                    break

        # Loop variable is private unless already set as firstprivate.
        # Throw exception if shared
        loop_var_ref = Reference(loop_var)
        if loop_var_ref not in self._parallel_private:
            raise GenerationError("Found shared loop variable which is"
                                  "not allowed in OpenMP Task directive. "
                                  f"Variable name is {loop_var_ref.name}")
        if loop_var_ref not in firstprivate_list:
            if loop_var_ref not in private_list:
                private_list.append(loop_var_ref)

        # If we have a proxy variable, the parent loop variable has to be
        # firstprivate
        if to_remove is not None:
            parent_var_ref =\
                Reference(self._proxy_loop_vars[to_remove]['parent_var'])
            if parent_var_ref not in firstprivate_list:
                firstprivate_list.append(parent_var_ref.copy())

        # For all non-array accesses we make them firstprivate unless they
        # are already declared as something else
        for ref in start_val_refs:
            if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
                raise GenerationError(f"{type(ref).__name__} not supported in "
                                      "the start variable of a Loop in a "
                                      "OMPTaskDirective node.")
            if (ref not in firstprivate_list and ref not in private_list and
                    ref not in shared_list):
                firstprivate_list.append(ref.copy())

        stop_val_refs = stop_val.walk(Reference)
        for ref in stop_val_refs:
            if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
                raise GenerationError(f"{type(ref).__name__} not supported in "
                                      "the stop variable of a Loop in a "
                                      "OMPTaskDirective node.")
            if (ref not in firstprivate_list and ref not in private_list and
                    ref not in shared_list):
                firstprivate_list.append(ref.copy())

        step_val_refs = step_val.walk(Reference)
        for ref in step_val_refs:
            if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
                raise GenerationError(f"{type(ref).__name__} not supported in "
                                      "the step variable of a Loop in a "
                                      "OMPTaskDirective node.")
            if (ref not in firstprivate_list and ref not in private_list and
                    ref not in shared_list):
                firstprivate_list.append(ref.copy())

        # Finished handling the loop bounds now

        # Recurse to the children
        for child_node in node.children[3].children:
            self._evaluate_node(child_node, private_list, firstprivate_list,
                                shared_list, in_list, out_list)

        # Remove any stuff added to proxy_loop_vars etc. if needed
        if to_remove is not None:
            self._proxy_loop_vars.pop(to_remove)

    def _evaluate_ifblock(self, node, private_list, firstprivate_list,
                          shared_list, in_list, out_list):
        '''
        Evaluates an ifblock inside a task region. This is done by calling
        _evaluate_readonly_reference on each Reference inside the if condition,
        and by calling _evaluate_node on each Node inside the if_body and
        else_body.

        :param node: The IfBlock to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.IfBlock`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        '''
        for ref in node.condition.walk(Reference):
            self._evaluate_readonly_reference(ref, private_list,
                                              firstprivate_list, shared_list,
                                              in_list)

        # Recurse to the children
        # If block
        for child_node in node.if_body.children:
            self._evaluate_node(child_node, private_list, firstprivate_list,
                                shared_list, in_list, out_list)
        # Else block if present
        if node.else_body is not None:
            for child_node in node.else_body.children:
                self._evaluate_node(child_node, private_list,
                                    firstprivate_list, shared_list,
                                    in_list, out_list)

    def _evaluate_node(self, node, private_list, firstprivate_list,
                       shared_list, in_list, out_list):
        '''
        Evaluates a generic Node inside the task region. Calls the appropriate
        call depending on whether the node is an Assignment, Loop or IfBlock.

        :param node: The Node to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        '''
        # For the node, check if it is Loop, Assignment or IfBlock
        if isinstance(node, Assignment):
            # Resolve assignment
            self._evaluate_assignment(node, private_list, firstprivate_list,
                                      shared_list, in_list, out_list)
        elif isinstance(node, Loop):
            # Resolve loop
            self._evaluate_loop(node, private_list, firstprivate_list,
                                shared_list, in_list, out_list)
        elif isinstance(node, IfBlock):
            # Resolve IfBlock
            self._evaluate_ifblock(node, private_list, firstprivate_list,
                                   shared_list, in_list, out_list)

        # All other node types are ignored (for now, maybe some error
        # checking might be useful, though I don't have rules on what isn't
        # allowed).

    def _compute_clauses(self):
        '''
        Computes the clauses for this OMPTaskDirective.

        The OMPTaskDirective must have exactly 1 child, which must be a Loop.
        Upon confirming this, the function calls _evaluate_node to compute all
        data-sharing attributes and dependencies.
        The clauses are then built up from those, and returned.

        :raises GenerationError: If the OMPTaskDirective has multiple children.
        :raises GenerationError: If the OMPTaskDirective's child is not a Loop.

        :returns: The clauses computed for this OMPTaskDirective.
        :rtype: List of [OMPPrivateClause, OMPFirstprivateClause,
                         OMPSharedClause, OMPDependClause, OMPDependClause]
        '''
        private_list = []
        firstprivate_list = []
        shared_list = []
        in_list = []
        out_list = []

        # Find all the parent loop variables
        self._find_parent_loop_vars()

        # Find the child loop node, and check our schedule contains a single
        # loop for now.
        if len(self.children[0].children) != 1:
            raise GenerationError("OMPTaskDirective must have exactly one Loop"
                                  f" child. Found "
                                  f"{len(self.children[0].children)} "
                                  "children.")
        if not isinstance(self.children[0].children[0], Loop):
            raise GenerationError("OMPTaskDirective must have exactly one Loop"
                                  " child. Found "
                                  f"{type(self.children[0].children[0])}")
        self._evaluate_node(self.children[0].children[0], private_list,
                            firstprivate_list, shared_list, in_list,
                            out_list)

        # Make the clauses to return.
        private_clause = OMPPrivateClause()
        for ref in private_list:
            private_clause.addchild(ref)
        firstprivate_clause = OMPFirstprivateClause()
        for ref in firstprivate_list:
            firstprivate_clause.addchild(ref)
        shared_clause = OMPSharedClause()
        for ref in shared_list:
            shared_clause.addchild(ref)

        in_clause = OMPDependClause(
                        depend_type=OMPDependClause.DependClauseTypes.IN)
        for ref in in_list:
            in_clause.addchild(ref)
        out_clause = OMPDependClause(
                        depend_type=OMPDependClause.DependClauseTypes.OUT)
        for ref in out_list:
            out_clause.addchild(ref)

        return (private_clause, firstprivate_clause, shared_clause, in_clause,
                out_clause)

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp task ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        private_clause, firstprivate_clause, shared_clause, in_clause,\
            out_clause = self._compute_clauses()

        if len(self.children) < 2 or private_clause != self.children[1]:
            self.children[1] = private_clause
        if len(self.children) < 3 or firstprivate_clause != self.children[2]:
            self.children[2] = firstprivate_clause
        if len(self.children) < 4 or shared_clause != self.children[3]:
            self.children[3] = shared_clause
        if len(self.children) < 5 or in_clause != self.children[4]:
            self.children[4] = in_clause
        if len(self.children) < 6 or out_clause != self.children[5]:
            self.children[5] = out_clause

        # Generate the string containing the required clauses
        return "omp task"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end task". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end task"


class OMPTaskloopDirective(OMPRegionDirective):
    '''
    Class representing an OpenMP TASKLOOP directive in the PSyIR.

    :param list children: list of Nodes that are children of this Node.
    :param parent: the Node in the AST that has this directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param grainsize: The grainsize value used to specify the grainsize \
                      clause on this OpenMP directive. If this is None \
                      the grainsize clause is not applied. Default \
                      value is None.
    :type grainsize: int or None.
    :param num_tasks: The num_tasks value used to specify the num_tasks \
                      clause on this OpenMP directive. If this is None \
                      the num_tasks clause is not applied. Default value \
                      is None.
    :type num_tasks: int or None.
    :param nogroup: Whether the nogroup clause should be used for this node. \
                    Default value is False
    :type nogroup: bool

    :raises GenerationError: if this OMPTaskloopDirective has both \
                             a grainsize and num_tasks value \
                             specified.
    '''
    # This specification respects the mutual exclusion of OMPGransizeClause
    # and OMPNumTasksClause, but adds an additional ordering requirement.
    # Other specifications to soften the ordering requirement are possible,
    # but need additional checks in the global constraints instead.
    _children_valid_format = ("Schedule, [OMPGrainsizeClause | "
                              "OMPNumTasksClause], [OMPNogroupClause]")

    # pylint: disable=too-many-arguments
    def __init__(self, children=None, parent=None, grainsize=None,
                 num_tasks=None, nogroup=False):
        # These remain primarily for the gen_code interface
        self._grainsize = grainsize
        self._num_tasks = num_tasks
        self._nogroup = nogroup
        if self._grainsize is not None and self._num_tasks is not None:
            raise GenerationError(
                "OMPTaskloopDirective must not have both grainsize and "
                "numtasks clauses specified.")
        super(OMPTaskloopDirective, self).__init__(children=children,
                                                   parent=parent)
        if self._grainsize is not None:
            child = [Literal(f"{grainsize}", INTEGER_TYPE)]
            self._children.append(OMPGrainsizeClause(children=child))
        if self._num_tasks is not None:
            child = [Literal(f"{num_tasks}", INTEGER_TYPE)]
            self._children.append(OMPNumTasksClause(children=child))
        if self._nogroup:
            self._children.append(OMPNogroupClause())

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         The rules are:
         1. Child 0 must always be a Schedule.
         2. Child 1 may be either a OMPGrainsizeClause or OMPNumTasksClause, \
            or if neither of those clauses are present, it may be a \
            OMPNogroupClause.
         3. Child 2 must always be a OMPNogroupClause, and can only exist if \
            child 1 is a OMPGrainsizeClause or OMPNumTasksClause.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Schedule)
        if position == 1:
            return isinstance(child, (OMPGrainsizeClause, OMPNumTasksClause,
                                      OMPNogroupClause))
        if position == 2:
            return isinstance(child, OMPNogroupClause)
        return False

    @property
    def nogroup(self):
        '''
        :returns: the nogroup clause status of this node.
        :rtype: bool
        '''
        return self._nogroup

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPTaskloopDirective is not \
                                 enclosed within an OpenMP serial region.
        :raises GenerationError: if this OMPTaskloopDirective has two
                                 Nogroup clauses as children.
        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). A taskloop directive, we must
        # have an OMPSerialDirective as an ancestor back up the tree.
        if not self.ancestor(OMPSerialDirective):
            raise GenerationError(
                "OMPTaskloopDirective must be inside an OMP Serial region "
                "but could not find an ancestor node")

        # Check children are well formed.
        # _validate_child will ensure position 0 and 1 are valid.
        if len(self._children) == 3 and isinstance(self._children[1],
                                                   OMPNogroupClause):
            raise GenerationError(
                "OMPTaskloopDirective has two Nogroup clauses as children "
                "which is not allowed.")

        super(OMPTaskloopDirective, self).validate_global_constraints()

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenMP
        taskloop directive.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if this "!$omp taskloop" is not enclosed \
                                 within an OMP Parallel region and an OMP \
                                 Serial region.

        '''
        self.validate_global_constraints()

        extra_clauses = ""
        # Find the specified clauses
        clause_list = []
        if self._grainsize is not None:
            clause_list.append(f"grainsize({self._grainsize})")
        if self._num_tasks is not None:
            clause_list.append(f"num_tasks({self._num_tasks})")
        if self._nogroup:
            clause_list.append("nogroup")
        # Generate the string containing the required clauses
        extra_clauses = ", ".join(clause_list)

        parent.add(DirectiveGen(parent, "omp", "begin", "taskloop",
                                extra_clauses))

        self.dir_body.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, "omp", "end", "taskloop", ""),
                   position=["after", position])

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp taskloop ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp taskloop"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end taskloop". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end taskloop"


class OMPDoDirective(OMPRegionDirective):
    '''
    Class representing an OpenMP DO directive in the PSyIR.

    :param list children: list of Nodes that are children of this Node.
    :param parent: the Node in the AST that has this directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param str omp_schedule: the OpenMP schedule to use.
    :param bool reprod: whether or not to generate code for run-reproducible \
                        OpenMP reductions.

    '''
    def __init__(self, children=None, parent=None, omp_schedule="static",
                 reprod=None):

        if reprod is None:
            self._reprod = Config.get().reproducible_reductions
        else:
            self._reprod = reprod

        self._omp_schedule = omp_schedule

        # Call the init method of the base class once we've stored
        # the OpenMP schedule
        super(OMPDoDirective, self).__init__(children=children,
                                             parent=parent)

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two OMPDoDirective nodes are equal
        if they have the same schedule, the same reproducible reduction option
        (and the inherited equality is True).

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.omp_schedule == other.omp_schedule
        is_eq = is_eq and self.reprod == other.reprod

        return is_eq

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        if self.reductions():
            reprod = f"reprod={self._reprod}"
        else:
            reprod = ""
        return f"{self.coloured_name(colour)}[{reprod}]"

    def _reduction_string(self):
        ''' Return the OMP reduction information as a string '''
        reduction_str = ""
        for reduction_type in AccessType.get_valid_reduction_modes():
            reductions = self._get_reductions_list(reduction_type)
            for reduction in reductions:
                reduction_str += (f", reduction("
                                  f"{OMP_OPERATOR_MAPPING[reduction_type]}:"
                                  f"{reduction})")
        return reduction_str

    @property
    def omp_schedule(self):
        '''
        :returns: the omp_schedule for this object.
        :rtype: str
        '''
        return self._omp_schedule

    @property
    def reprod(self):
        ''' returns whether reprod has been set for this object or not '''
        return self._reprod

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPDoDirective is not enclosed \
                            within some OpenMP parallel region.
        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As a loop
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                "OMPDoDirective must be inside an OMP parallel region but "
                "could not find an ancestor OMPParallelDirective node")

        self._validate_single_loop()

        super(OMPDoDirective, self).validate_global_constraints()

    def _validate_single_loop(self):
        '''
        Checks that this directive is only applied to a single Loop node.

        :raises GenerationError: if this directive has more than one child.
        :raises GenerationError: if the child of this directive is not a Loop.

        '''
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                f"An {type(self).__name__} can only be applied to a single "
                f"loop but this Node has {len(self.dir_body.children)} "
                f"children: {self.dir_body.children}")

        if not isinstance(self.dir_body[0], Loop):
            raise GenerationError(
                f"An {type(self).__name__} can only be applied to a loop but "
                f"this Node has a child of type "
                f"'{type(self.dir_body[0]).__name__}'")

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenMP do
        directive.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if this "!$omp do" is not enclosed within \
                                 an OMP Parallel region.

        '''
        self.validate_global_constraints()

        if self._reprod:
            local_reduction_string = ""
        else:
            local_reduction_string = self._reduction_string()

        # As we're a loop we don't specify the scope
        # of any variables so we don't have to generate the
        # list of private variables
        options = f"schedule({self._omp_schedule}){local_reduction_string}"
        parent.add(DirectiveGen(parent, "omp", "begin", "do", options))

        for child in self.children:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, "omp", "end", "do", ""),
                   position=["after", position])

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp do ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        return f"omp do schedule({self._omp_schedule})"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end do". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end do"


class OMPParallelDoDirective(OMPParallelDirective, OMPDoDirective):
    ''' Class for the !$OMP PARALLEL DO directive. This inherits from
        both OMPParallelDirective (because it creates a new OpenMP
        thread-parallel region) and OMPDoDirective (because it
        causes a loop to be parallelised). '''

    _children_valid_format = ("Schedule, OMPDefaultClause, OMPPrivateClause, "
                              "OMPScheduleClause, [OMPReductionClause]*")

    def __init__(self, children=[], parent=None, omp_schedule="static"):
        OMPDoDirective.__init__(self,
                                children=children,
                                parent=parent,
                                omp_schedule=omp_schedule)
        self.addchild(OMPDefaultClause(
            clause_type=OMPDefaultClause.DefaultClauseTypes.SHARED))

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0 and isinstance(child, Schedule):
            return True
        if position == 1 and isinstance(child, OMPDefaultClause):
            return True
        if position == 2 and isinstance(child, OMPPrivateClause):
            return True
        if position == 3 and isinstance(child, OMPScheduleClause):
            return True
        if position >= 4 and isinstance(child, OMPReductionClause):
            return True
        return False

    def gen_code(self, parent):

        # We're not doing nested parallelism so make sure that this
        # omp parallel do is not already within some parallel region
        from psyclone.psyGen import zero_reduction_variables
        self.validate_global_constraints()

        calls = self.reductions()
        zero_reduction_variables(calls, parent)
        private_clause = self._get_private_clause()
        if len(self._children) >= 3 and private_clause != self._children[2]:
            # Replace the current private clause.
            self._children[2] = private_clause
        elif len(self._children) < 3:
            self.addchild(private_clause, index=2)
        default_str = self.children[1]._clause_string
        private_list = []
        for child in self.children[2].children:
            private_list.append(child.symbol.name)
        private_str = "private(" + ",".join(private_list) + ")"

        sched_clause = OMPScheduleClause(self._omp_schedule)
        if len(self._children) >= 4 and sched_clause != self._children[3]:
            self._children[3] = sched_clause
        elif len(self._children) < 4:
            self.addchild(sched_clause, index=3)
        schedule_str = f"schedule({sched_clause.schedule})"
        parent.add(DirectiveGen(parent, "omp", "begin", "parallel do",
                                f"{default_str}, {private_str}, {schedule_str}"
                                f"{self._reduction_string()}"))

        for child in self.dir_body:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, *self.end_string().split()),
                   position=["after", position])

        self.gen_post_region_code(parent)

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp do ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        private_clause = self._get_private_clause()
        if len(self._children) >= 3 and private_clause != self._children[2]:
            self._children[2] = private_clause
        elif len(self._children) < 3:
            self.addchild(private_clause, index=2)
        sched_clause = OMPScheduleClause(self._omp_schedule)
        if len(self._children) >= 4 and sched_clause != self._children[3]:
            self._children[3] = sched_clause
        elif len(self._children) < 4:
            self.addchild(sched_clause, index=3)
        return ("omp parallel do" + self._reduction_string())

    def end_string(self):
        '''
        :returns: the closing statement for this directive.
        :rtype: str
        '''
        # pylint: disable=no-self-use
        return "omp end parallel do"

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        '''
        super(OMPParallelDoDirective, self).validate_global_constraints()

        self._validate_single_loop()


class OMPTargetDirective(OMPRegionDirective):
    ''' Class for the !$OMP TARGET directive that offloads the code contained
    in its region into an accelerator device. '''

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp target". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp target"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end target". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end target"


class OMPLoopDirective(OMPRegionDirective):
    ''' Class for the !$OMP LOOP directive that specifies that the iterations
    of the associated loops may execute concurrently.

    :param int collapse: optional number of nested loops to collapse into a \
        single iteration space to parallelise. Defaults to None.
    '''

    def __init__(self, collapse=None, **kwargs):
        super(OMPLoopDirective, self).__init__(**kwargs)
        self._collapse = None
        self.collapse = collapse  # Use setter with error checking

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two OMPLoopDirective nodes are
        equal if they have the same collapse status and the inherited
        equality is true.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.collapse == other.collapse

        return is_eq

    @property
    def collapse(self):
        '''
        :returns: the value of the collapse clause.
        :rtype: int or NoneType
        '''
        return self._collapse

    @collapse.setter
    def collapse(self, value):
        '''
        :param value: optional number of nested loop to collapse into a \
            single iteration space to parallelise. Defaults to None.
        :type value: int or NoneType.

        :raises TypeError: if the collapse value given is not an integer \
            or NoneType.
        :raises ValueError: if the collapse integer given is not positive.

        '''
        if value is not None and not isinstance(value, int):
            raise TypeError(
                f"The OMPLoopDirective collapse clause must be a positive "
                f"integer or None, but value '{value}' has been given.")

        if value is not None and value <= 0:
            raise ValueError(
                f"The OMPLoopDirective collapse clause must be a positive "
                f"integer or None, but value '{value}' has been given.")

        self._collapse = value

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        text = self.coloured_name(colour)
        if self._collapse:
            text += f"[collapse={self._collapse}]"
        else:
            text += "[]"
        return text

    def begin_string(self):
        ''' Returns the beginning statement of this directive, i.e. "omp loop".
        The visitor is responsible for adding the correct directive beginning
        (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        string = "omp loop"
        if self._collapse:
            string += f" collapse({self._collapse})"
        return string

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end loop". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end loop"

    def validate_global_constraints(self):
        ''' Perform validation of those global constraints that can only be
        done at code-generation time.

        :raises GenerationError: if this OMPLoopDirective has more than one \
            child in its associated schedule.
        :raises GenerationError: if the schedule associated with this \
            OMPLoopDirective does not contain a Loop.
        :raises GenerationError: this directive must be inside a omp target \
            or parallel region.
        :raises GenerationError: if this OMPLoopDirective has a collapse \
            clause but it doesn't have the expected number of nested Loops.

        '''
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                f"OMPLoopDirective must have exactly one child in its "
                f"associated schedule but found {self.dir_body.children}.")

        if not isinstance(self.dir_body.children[0], Loop):
            raise GenerationError(
                f"OMPLoopDirective must have a Loop as child of its associated"
                f" schedule but found '{self.dir_body.children[0]}'.")

        if not self.ancestor((OMPTargetDirective, OMPParallelDirective)):
            # Also omp teams or omp threads regions but these are not supported
            # in the PSyIR
            raise GenerationError(
                f"OMPLoopDirective must be inside a OMPTargetDirective or a "
                f"OMPParallelDirective, but '{self}' is not.")

        # If there is a collapse clause, there must be as many immediately
        # nested loops as the collapse value
        if self._collapse:
            cursor = self.dir_body.children[0]
            for depth in range(self._collapse):
                if not isinstance(cursor, Loop):
                    raise GenerationError(
                        f"OMPLoopDirective must have as many immediately "
                        f"nested loops as the collapse clause specifies but "
                        f"'{self}' has a collapse={self._collapse} and the "
                        f"nested statement at depth {depth} is a "
                        f"{type(cursor).__name__} rather than a Loop.")
                cursor = cursor.loop_body.children[0]

        super(OMPLoopDirective, self).validate_global_constraints()


# For automatic API documentation generation
__all__ = ["OMPRegionDirective", "OMPParallelDirective", "OMPSingleDirective",
           "OMPMasterDirective", "OMPDoDirective", "OMPParallelDoDirective",
           "OMPSerialDirective", "OMPTaskloopDirective", "OMPTargetDirective",
           "OMPTaskwaitDirective", "OMPDirective", "OMPStandaloneDirective",
           "OMPLoopDirective", "OMPDeclareTargetDirective"]
