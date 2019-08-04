import React from "react"
import { Row, Col, Button } from "react-bootstrap"

export const Login = props =>
  <div>
      <Row>
        <Col md={3}>Login!</Col>
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
           onChange={e => props.setPassword(e.target.value) }/>
        </Col>
      </Row>
      <Row>
        <Col md={3}><Button onClick={props.submit} block>Login</Button></Col>
      </Row>
      <Row>
        <Col md={3}>Need to Register?</Col>
        <Col md={3}><Button onClick={props.register} block>Register</Button></Col>
      </Row>
  </div>
