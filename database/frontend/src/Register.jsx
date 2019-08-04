import React from "react"
import { Row, Col, Button } from "react-bootstrap"

export const Register = props =>
  <div>
      <Row>
        <Col md={3}>Register!</Col>
      </Row>
      <Row>
        <Col md={3}>Name</Col>
        <Col md={9}>
          <input id="name" type="text" name="name" class="border-solid border-4 border-gray-600 ..."
           onChange={e => props.setName(e.target.value) }/>
        </Col>
      </Row>
      <Row>
        <Col md={3}>Email</Col>
        <Col md={9}>
          <input id="email" type="text" name="email" class="border-solid border-4 border-gray-600 ..."
           onChange={e => props.setEmail(e.target.value) }/>
        </Col>
      </Row>
      <Row>
        <Col md={3}>Password</Col>
        <Col md={9}>
          <input id="name" type="password" name="name" class="border-solid border-4 border-gray-600 ..."
           onChange={e => props.setPassword1(e.target.value) }/>
        </Col>
      </Row>
      <Row>
        <Col md={3}>Password Again</Col>
        <Col md={9}>
          <input id="name" type="password" name="name" class="border-solid border-4 border-gray-600 ..."
           onChange={e => props.setPassword2(e.target.value) }/>
        </Col>
      </Row>
      <Row>
        <Col md={3}><Button onClick={props.submit} block>Register</Button></Col>
      </Row>
  </div>
