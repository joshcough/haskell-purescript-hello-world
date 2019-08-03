import React from "react"
import { Row, Col, Button } from "react-bootstrap"

export const Counter = props =>
  <Row>
    <Col md={3}>
      Enter a number:
      <input id="myInput" type="text" name="name" class="border-solid border-4 border-gray-600 ..."
       onChange={e => props.setCount(e.target.value) }/>
    </Col>
    <Col md={3}>
      <Button onClick={props.getMessage} block>Get Message</Button>
    </Col>
  </Row>
