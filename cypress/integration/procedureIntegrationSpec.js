describe("Procedures", () => {
  it("runs a procedure from upon init", () => {
    cy.visit("http://localhost:9732")

    cy.get("body")
      .type("bbb")

    cy.get("[data-on-type]")
      .should('contain', "You have not yet pressed Z")

    cy.get("body")
      .type("Z")

    cy.get("[data-on-type]")
      .should('contain', "You pressed Z!!!")
  })

  it("runs a procedure that makes a request to a port and processes the response", () => {
    cy.visit("http://localhost:9732")

    cy.get("[data-port-input]")
      .type("Hello!")

    cy.get("[data-port-submit]")
      .click()

    cy.get("[data-port-message]")
      .should('contain', 'Thanks for the message: Hello!')
  })
})