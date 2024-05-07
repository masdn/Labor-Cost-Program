CREATE TABLE inventory(
    id SERIAL PRIMARY KEY,
    sku TEXT NOT NULL,
    size INT,
    price INT, 
    description TEXT,
    UNIQUE (sku)
);

CREATE TABLE category (
    id SERIAL PRIMARY KEY,
    label TEXT NOT NULL,
    UNIQUE (id)
);

CREATE TABLE product_category(
    category_id INT not NULL,
    product_id INT NOT NULL,
    PRIMARY KEY (category_id, product_id),
    FOREIGN KEY (product_id) REFERENCES inventory(id),
    FOREIGN KEY (category_id) REFERENCES category(id)
);

CREATE TABLE purchase_orders (
    po_id SERIAL PRIMARY KEY, 
    product_id INT NOT NULL,
    quantity INT NOT NULL,
    subtotal INT,
    labor_cost INT,
    created TIMESTAMP,
    modified TIMESTAMP,
    FOREIGN KEY (product_id) REFERENCES inventory(id)
);
