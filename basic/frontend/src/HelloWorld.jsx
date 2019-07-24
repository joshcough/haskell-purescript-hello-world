import React from "react"
import { Row, Col, Button, Alert, Card } from "react-bootstrap"

export const Counter = props =>
  <Row>
    <Col md={3}>
      <Button onClick={props.getMessage} block>Get Message</Button>
    </Col>
  </Row>
