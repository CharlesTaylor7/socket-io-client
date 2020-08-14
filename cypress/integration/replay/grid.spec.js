
describe('Grid', () => {
  beforeEach(() => {
    cy.visit('')
  })

  it('has square tiles of uniform size', () => {
    cy.get('.replay-dropdown')
      .select('1')

    cy.get('.grid')
      .trigger('wheel', 'center', {
        force: true,
        deltaY: -500,
      })
      .find('td')
      .then($tableCells => {
        console.log($tableCells)
        for (let $cell of $tableCells) {
          cy.wrap($cell)
            .should('have.css', 'width', '30.4px')
            .should('have.css', 'height', '30.4px')
        }
      })
  })
})
